#' @title Hybrid OpenAlex + OpenCitations crawler
#' @description Port of the original Python `retraction_pollution.crawler`
#'   module. Resolves pending seeds against OpenAlex (by DOI, PMID, or
#'   title fallback) and crawls the citation graph by depth, supplementing
#'   OpenAlex's `cites` filter with OpenCitations when available.

library(R6)

#' Resolve pending seeds against OpenAlex.
#'
#' @param store `StudyStore` instance.
#' @param client `OpenAlexClient`-like object.
#' @param limit integer max seeds to process, or `NA` for no limit.
#' @param batch_size integer DOI batch size (clamped to 100).
#' @param title_fallback logical; if `TRUE`, attempt title-author search for
#'   seeds lacking DOI/PMID and mark unresolved ones `not_found`. If `FALSE`,
#'   seeds lacking DOI/PMID are marked `pending_title_fallback` and skipped.
#' @return named list of stats: `resolved`, `not_found`, `checked`,
#'   `pending_title_fallback`.
resolve_pending_seeds <- function(store, client, limit = NA, batch_size = 100,
                                  title_fallback = FALSE) {
  seeds <- store$unresolved_seeds(limit = limit,
                                 include_pending_title_fallback = title_fallback)
  stats <- list(resolved = 0L, not_found = 0L, checked = 0L,
                pending_title_fallback = 0L)

  if (!isTRUE(title_fallback)) {
    seeds_no_id <- Filter(function(s) {
      is.na(clean_doi(s$original_doi)) && is.na(clean_pmid(s$original_pmid))
    }, seeds)
    for (seed in seeds_no_id) {
      store$update_seed_resolution(seed$record_id, NA, "exact",
                                   "pending_title_fallback")
      stats$pending_title_fallback <- stats$pending_title_fallback + 1L
    }
    seeds <- Filter(function(s) {
      !is.na(clean_doi(s$original_doi)) || !is.na(clean_pmid(s$original_pmid))
    }, seeds)
  }

  doi_seeds <- Filter(function(s) !is.na(clean_doi(s$original_doi)), seeds)
  for (batch in chunked(doi_seeds, min(as.integer(batch_size), 100L))) {
    dois <- vapply(batch, function(s) s$original_doi, character(1))
    works <- client$get_works_by_dois(dois)
    by_doi <- new.env(hash = TRUE, size = max(1L, length(works)))
    for (work in works) {
      doi <- clean_doi(work$doi)
      if (!is.na(doi)) assign(doi, work, envir = by_doi)
    }
    for (seed in batch) {
      stats$checked <- stats$checked + 1L
      work <- tryCatch(get(clean_doi(seed$original_doi), envir = by_doi),
                       error = function(e) NULL)
      if (!is.null(work) && !is.null(work$id) && !is.na(work$id)) {
        row <- normalize_work(work)
        store$upsert_work(row)
        store$update_seed_resolution(seed$record_id, row$openalex_id,
                                      "doi", "resolved")
        stats$resolved <- stats$resolved + 1L
      }
    }
  }

  remaining <- store$unresolved_seeds(limit = limit,
                                     include_pending_title_fallback = title_fallback)
  for (seed in remaining) {
    pmid <- clean_pmid(seed$original_pmid)
    if (!is.na(pmid)) {
      stats$checked <- stats$checked + 1L
      work <- client$get_work_by_pmid(pmid)
      if (!is.null(work) && !is.null(work$id) && !is.na(work$id)) {
        row <- normalize_work(work)
        store$upsert_work(row)
        store$update_seed_resolution(seed$record_id, row$openalex_id,
                                      "pmid", "resolved")
        stats$resolved <- stats$resolved + 1L
        next
      }
    }
    if (isTRUE(title_fallback)) {
      terms <- search_fallback_terms(seed)
      if (!is.na(terms$title)) {
        stats$checked <- stats$checked + 1L
        work <- client$search_work(terms$title, terms$author_last_name)
        if (!is.null(work) && !is.null(work$id) && !is.na(work$id)) {
          row <- normalize_work(work)
          store$upsert_work(row)
          store$update_seed_resolution(seed$record_id, row$openalex_id,
                                        "title_author", "resolved")
          stats$resolved <- stats$resolved + 1L
          next
        }
      }
    }
    if (isTRUE(title_fallback)) {
      store$update_seed_resolution(seed$record_id, NA, "all", "not_found")
      stats$not_found <- stats$not_found + 1L
    } else {
      store$update_seed_resolution(seed$record_id, NA, "exact",
                                   "pending_title_fallback")
      stats$pending_title_fallback <- stats$pending_title_fallback + 1L
    }
  }

  stats
}

#' @title Citation graph crawler
#' @description R6 class crawling the citation graph outward from resolved
#'   seed nodes, depth by depth, using OpenAlex's `cites` filter optionally
#'   supplemented by OpenCitations for DOI-discovered parents.
#' @noRd
CitationCrawler <- R6::R6Class(
  "CitationCrawler",
  cloneable = FALSE,
  public = list(
    store = NULL,
    openalex = NULL,
    opencitations = NULL,

    initialize = function(store, openalex, opencitations = NULL) {
      self$store <- store
      self$openalex <- openalex
      self$opencitations <- opencitations
    },

    supplement_level_from_opencitations = function(current_depth, target_depth,
                                                  batch_size = 100,
                                                  parent_limit = NA,
                                                  depth3_node_cap = 250000L) {
      stats <- list(parents = 0L, citations = 0L,
                    resolved_depth_nodes = 0L, truncated = FALSE)
      if (is.null(self$opencitations)) return(stats)
      parents <- self$store$frontier_with_doi(current_depth)
      if (!is_missing(parent_limit)) {
        parents <- parents[seq_len(min(length(parents), as.integer(parent_limit)))]
      }
      for (parent in parents) {
        if (target_depth == 3L &&
            self$store$count_frontier_depth(3L) >= as.integer(depth3_node_cap)) {
          stats$truncated <- TRUE
          self$store$set_metadata("depth3_truncated", "true")
          break
        }
        stats$parents <- stats$parents + 1L
        citations <- self$opencitations$citations_by_doi(parent$doi)
        stats$citations <- stats$citations + length(citations)
        private$impl_store_opencitations_for_parent(
          parent$openalex_id, citations, target_depth, batch_size,
          depth3_node_cap = depth3_node_cap
        )
        stats$resolved_depth_nodes <- self$store$count_frontier_depth(target_depth)
      }
      self$store$set_metadata(
        paste0("opencitations_depth", target_depth, "_supplement"),
        stats
      )
      stats
    },

    store_opencitations_for_parent = function(parent_openalex_id, citations,
                                              target_depth, batch_size,
                                              depth3_node_cap) {
      private$impl_store_opencitations_for_parent(
        parent_openalex_id, citations, target_depth, batch_size,
        depth3_node_cap = depth3_node_cap
      )
    },

    crawl = function(max_depth = 3L, complete_depth = 2L, batch_size = 100L,
                     per_page = 100L, depth3_node_cap = 250000L,
                     depth3_page_cap = 2500L) {
      seed_ids <- self$store$resolved_seed_ids()
      for (seed_id in seed_ids) {
        self$store$add_frontier_node(seed_id, 0L)
      }

      summary <- list(
        seed_nodes = length(seed_ids),
        max_depth = as.integer(max_depth),
        complete_depth = as.integer(complete_depth),
        levels = list()
      )
      depth3_truncated <- FALSE

      for (current_depth in seq_len(as.integer(max_depth)) - 1L) {
        target_depth <- current_depth + 1L
        capped <- target_depth > as.integer(complete_depth)
        oc_stats <- self$supplement_level_from_opencitations(
          current_depth = current_depth,
          target_depth = target_depth,
          batch_size = batch_size,
          depth3_node_cap = depth3_node_cap
        )
        if (isTRUE(oc_stats$truncated)) {
          summary$levels[[as.character(target_depth)]] <-
            list(opencitations = oc_stats)
          if (target_depth >= 3L) depth3_truncated <- TRUE
          break
        }
        level_stats <- private$impl_crawl_level(
          current_depth, target_depth, capped = capped,
          batch_size = batch_size, per_page = per_page,
          depth3_node_cap = depth3_node_cap,
          depth3_page_cap = depth3_page_cap
        )
        level_stats$opencitations <- oc_stats
        summary$levels[[as.character(target_depth)]] <- level_stats
        if (isTRUE(level_stats$truncated)) {
          if (target_depth >= 3L) depth3_truncated <- TRUE
          break
        }
      }

      self$store$set_metadata("last_crawl_summary", summary)
      self$store$set_metadata("depth3_truncated",
                              if (depth3_truncated) "true" else "false")
      summary
    },

    crawl_level = function(current_depth, target_depth, capped, batch_size,
                          per_page, depth3_node_cap, depth3_page_cap) {
      private$impl_crawl_level(current_depth, target_depth, capped = capped,
                          batch_size = batch_size, per_page = per_page,
                          depth3_node_cap = depth3_node_cap,
                          depth3_page_cap = depth3_page_cap)
    },

    store_page = function(works, parent_set, target_depth) {
      private$impl_store_page(works, parent_set, target_depth)
    },

    depth_cap_reached = function(target_depth, depth_node_cap, depth_page_cap) {
      private$impl_depth_cap_reached(target_depth, depth_node_cap, depth_page_cap)
    },

    increment_page_count = function(target_depth) {
      private$impl_increment_page_count(target_depth)
    }
  ),

  private = list(
    impl_store_opencitations_for_parent = function(parent_openalex_id, citations,
                                               target_depth, batch_size,
                                               depth3_node_cap) {
      by_doi <- new.env(hash = TRUE, size = max(1L, length(citations)))
      for (citation in citations) {
        if (!is.na(citation$citing_doi)) {
          assign(citation$citing_doi, citation, envir = by_doi)
        }
      }
      dois <- ls(by_doi)
      for (doi_batch in chunked(dois, min(as.integer(batch_size), 100L))) {
        if (target_depth == 3L &&
            self$store$count_frontier_depth(3L) >= as.integer(depth3_node_cap)) {
          return(invisible(NULL))
        }
        works <- self$openalex$get_works_by_dois(doi_batch)
        for (work in works) {
          row <- normalize_work(work)
          source_id <- row$openalex_id
          citing_doi <- clean_doi(row$doi)
          if (is.na(source_id) || is.na(citing_doi)) next
          citation <- tryCatch(get(citing_doi, envir = by_doi),
                               error = function(e) NULL)
          self$store$upsert_work(row)
          self$store$add_frontier_node(source_id, target_depth)
          self$store$add_edge(
            source_id, compact_openalex_id(parent_openalex_id),
            target_depth, source_api = "opencitations",
            citation_date = if (!is.null(citation)) citation$creation_date else NA
          )
        }
      }

      pmid_citations <- Filter(function(c) {
        is.na(c$citing_doi) && !is.na(c$citing_pmid)
      }, citations)
      for (citation in pmid_citations) {
        if (target_depth == 3L &&
            self$store$count_frontier_depth(3L) >= as.integer(depth3_node_cap)) {
          return(invisible(NULL))
        }
        work <- self$openalex$get_work_by_pmid(citation$citing_pmid)
        if (is.null(work)) next
        row <- normalize_work(work)
        source_id <- row$openalex_id
        if (is.na(source_id)) next
        self$store$upsert_work(row)
        self$store$add_frontier_node(source_id, target_depth)
        self$store$add_edge(
          source_id, compact_openalex_id(parent_openalex_id),
          target_depth, source_api = "opencitations",
          citation_date = citation$creation_date
        )
      }
      invisible(NULL)
    },

    impl_crawl_level = function(current_depth, target_depth, capped, batch_size,
                          per_page, depth3_node_cap, depth3_page_cap) {
      parents <- self$store$pending_frontier(current_depth)
      stats <- list(
        parent_count = length(parents),
        pages = 0L,
        results = 0L,
        new_depth_count_start = self$store$count_frontier_depth(target_depth),
        truncated = FALSE
      )
      for (parent_batch in chunked(parents, min(as.integer(batch_size), 100L))) {
        if (isTRUE(capped) &&
            private$impl_depth_cap_reached(target_depth, depth3_node_cap,
                                       depth3_page_cap)) {
          stats$truncated <- TRUE
          self$store$set_metadata(
            paste0("depth", target_depth, "_truncated"), "true"
          )
          break
        }
        job_id <- paste0("depth", current_depth, "-",
                         stable_hash(parent_batch))
        job <- self$store$get_or_create_job(job_id, current_depth, parent_batch)
        if (isTRUE(job$done)) next

        parent_set <- vapply(parent_batch, compact_openalex_id, character(1))
        parent_set <- parent_set[!is.na(parent_set) & parent_set != ""]
        cursor <- if (!is_missing(job$cursor) && !is.na(job$cursor) &&
                      job$cursor != "") job$cursor else "*"

        repeat {
          if (isTRUE(capped) &&
              private$impl_depth_cap_reached(target_depth, depth3_node_cap,
                                         depth3_page_cap)) {
            stats$truncated <- TRUE
            self$store$set_metadata(
              paste0("depth", target_depth, "_truncated"), "true"
            )
            return(stats)
          }
          page <- self$openalex$list_citers(parent_batch, cursor = cursor,
                                            per_page = per_page)
          private$impl_store_page(page$results, parent_set, target_depth)
          stats$pages <- stats$pages + 1L
          stats$results <- stats$results + length(page$results)
          if (isTRUE(capped)) private$impl_increment_page_count(target_depth)

          if (!is.null(page$next_cursor) && !is.na(page$next_cursor) &&
              length(page$results) > 0L) {
            cursor <- page$next_cursor
            self$store$update_job(job_id, cursor = cursor,
                                  pages_delta = 1L,
                                  results_delta = length(page$results))
            next
          }
          self$store$update_job(
            job_id,
            cursor = if (!is.null(page$next_cursor) && !is.na(page$next_cursor))
                       page$next_cursor else cursor,
            done = TRUE,
            pages_delta = 1L,
            results_delta = length(page$results)
          )
          self$store$mark_processed(parent_batch)
          break
        }
      }
      stats$new_depth_count_end <- self$store$count_frontier_depth(target_depth)
      stats
    },

    impl_store_page = function(works, parent_set, target_depth) {
      parent_ids <- parent_set[!is.na(parent_set) & parent_set != ""]
      for (work in works) {
        row <- normalize_work(work)
        source_id <- row$openalex_id
        if (is.na(source_id) || source_id == "") next
        self$store$upsert_work(row)
        self$store$add_frontier_node(source_id, target_depth)
        for (edge in edges_from_work(work, parent_ids)) {
          self$store$add_edge(edge[1], edge[2], target_depth,
                              source_api = "openalex")
        }
      }
      invisible(NULL)
    },

    impl_depth_cap_reached = function(target_depth, depth_node_cap, depth_page_cap) {
      if (target_depth != 3L) return(FALSE)
      nodes <- self$store$count_frontier_depth(target_depth) >=
        as.integer(depth_node_cap)
      pages <- suppressWarnings(as.integer(
        self$store$get_metadata(paste0("depth", target_depth, "_pages"),
                                "0") %||% "0"
      ))
      if (is.na(pages)) pages <- 0L
      isTRUE(nodes) || pages >= as.integer(depth_page_cap)
    },

    impl_increment_page_count = function(target_depth) {
      key <- paste0("depth", target_depth, "_pages")
      pages <- suppressWarnings(as.integer(
        self$store$get_metadata(key, "0") %||% "0"
      ))
      if (is.na(pages)) pages <- 0L
      self$store$set_metadata(key, as.character(pages + 1L))
    }
  )
)
