#' @title OpenCitations-only crawl pipeline
#' @description Port of the original Python `retraction_pollion.opencitations_pipeline`.
#'   Loads Retraction Watch seeds into the DuckDB store using DOI-derived node
#'   IDs, then crawls the OpenCitations citation graph breadth-first up to a
#'   configurable max depth.

library(R6)

#' Extract a 4-digit year from a date string.
#' @param value character (possibly `NA`).
#' @return integer year or `NA_integer_`.
#' @noRd
`_year` <- function(value) {
  text <- text_or_none(value)
  if (is.na(text)) return(NA_integer_)
  parsed <- parse_date(text)
  if (is.na(parsed)) return(NA_integer_)
  as.integer(substr(parsed, 1, 4))
}

#' Quick SQL check: is the given node_id also a seed openalex_id?
#' @param store StudyStore instance.
#' @param node_id character node ID.
#' @return logical.
is_seed_id <- function(store, node_id) {
  if (is_missing(node_id)) return(FALSE)
  df <- DBI::dbGetQuery(
    store$con,
    "SELECT 1 FROM seeds WHERE openalex_id = ? LIMIT 1",
    params = list(node_id)
  )
  nrow(df) > 0L
}

#' Load seeds into the store and resolve them to OpenCitations DOI node IDs.
#'
#' @param store StudyStore instance.
#' @param seeds list of seed named-lists (output of `load_seed_rows`).
#' @return list with named integer elements `loaded`, `doi_seeds`, `no_doi`.
prepare_opencitations_seeds <- function(store, seeds) {
  partitions <- partition_seed_list_by_doi(seeds)
  stats <- list(
    loaded = length(seeds),
    doi_seeds = 0L,
    no_doi = length(partitions$no_doi),
    duplicate_doi = length(partitions$duplicates)
  )

  for (seed in partitions$no_doi) {
    store$upsert_seed(seed)
    store$update_seed_resolution(seed$record_id, NA_character_,
                                  "opencitations_doi", "no_doi")
  }

  for (seed in partitions$duplicates) {
    store$upsert_seed(seed)
    node_id <- doi_node_id(clean_doi(seed$original_doi))
    store$update_seed_resolution(seed$record_id, node_id,
                                  "opencitations_doi", "duplicate_doi")
  }

  for (seed in partitions$canonical) {
    store$upsert_seed(seed)
    doi <- clean_doi(seed$original_doi)
    node_id <- doi_node_id(doi)

    raw_json <- if (!is_missing(seed$source_row_json)) seed$source_row_json
                else json_dumps(seed)

    store$upsert_work(list(
      openalex_id = node_id,
      doi = doi,
      title = seed$title,
      publication_date = seed$original_paper_date,
      publication_year = `_year`(seed$original_paper_date),
      work_type = seed$article_type,
      is_retracted = TRUE,
      cited_by_count = NA_integer_,
      source_id = NA_character_,
      source_name = seed$journal,
      topic_id = NA_character_,
      topic_name = seed$subject,
      topic_domain = NA_character_,
      referenced_works_json = "[]",
      raw_json = raw_json
    ))

    store$update_seed_resolution(seed$record_id, node_id,
                                 "opencitations_doi", "resolved")
    stats$doi_seeds <- stats$doi_seeds + 1L
  }

  store$set_metadata("pipeline_mode", "opencitations")
  store$set_metadata("opencitations_seed_stats", stats)
  stats
}

#' OpenCitations-only breadth-first citation crawler.
#'
#' R6 class. `crawl()` walks the citation graph one depth at a time, querying
#' OpenCitations for each pending frontier parent and recording new edges and
#' frontier nodes for each citing work discovered.
#' @noRd
OpenCitationsOnlyCrawler <- R6::R6Class(
  "OpenCitationsOnlyCrawler",
  cloneable = FALSE,
  public = list(
    store = NULL,
    client = NULL,

    initialize = function(store, client) {
      self$store <- store
      self$client <- client
    },

    crawl = function(max_depth = 3L, complete_depth = 2L,
                     depth3_node_cap = 250000L, parent_limit = NA) {
      seed_ids <- self$store$resolved_seed_ids()
      for (seed_id in seed_ids) {
        self$store$add_frontier_node(seed_id, 0L)
      }

      summary <- list(
        mode = "opencitations",
        seed_nodes = length(seed_ids),
        max_depth = max_depth,
        complete_depth = complete_depth,
        levels = list()
      )
      depth3_truncated <- FALSE

      for (current_depth in seq_len(max_depth) - 1L) {
        target_depth <- current_depth + 1L
        capped <- target_depth > complete_depth
        stats <- self$crawl_level(
          current_depth = current_depth,
          target_depth = target_depth,
          capped = capped,
          depth3_node_cap = depth3_node_cap,
          parent_limit = parent_limit
        )
        summary$levels[[as.character(target_depth)]] <- stats
        if (isTRUE(stats$truncated)) {
          if (target_depth >= 3L) depth3_truncated <- TRUE
          break
        }
      }

      self$store$set_metadata("last_crawl_summary", summary)
      self$store$set_metadata("depth3_truncated",
                              if (depth3_truncated) "true" else "false")
      summary
    },

    crawl_level = function(current_depth, target_depth, capped,
                            depth3_node_cap, parent_limit) {
      parents <- self$store$pending_frontier(current_depth, limit = parent_limit)

      stats <- list(
        parent_count = length(parents),
        parents_queried = 0L,
        parents_failed = 0L,
        parents_without_doi = 0L,
        citations = 0L,
        new_depth_count_start = self$store$count_frontier_depth(target_depth),
        new_depth_count_end = NA_integer_,
        truncated = FALSE
      )

      for (parent_id in parents) {
        if (isTRUE(capped) &&
            self$store$count_frontier_depth(target_depth) >= depth3_node_cap) {
          stats$truncated <- TRUE
          self$store$set_metadata(
            paste0("depth", target_depth, "_truncated"), TRUE
          )
          break
        }

        doi <- doi_from_node_id(parent_id)
        if (is.na(doi)) {
          stats$parents_without_doi <- stats$parents_without_doi + 1L
          self$store$mark_processed(list(parent_id))
          next
        }

        citations <- tryCatch(
          self$client$citations_by_doi(doi),
          OpenCitationsError = function(e) {
            stats$parents_failed <- stats$parents_failed + 1L
            self$store$set_metadata(
              paste0("opencitations_failed_parent:", parent_id),
              conditionMessage(e)
            )
            NULL
          }
        )

        if (is.null(citations)) next

        stats$parents_queried <- stats$parents_queried + 1L
        stats$citations <- stats$citations + length(citations)

        self$store_citations(parent_id, citations, target_depth)
        self$store$mark_processed(list(parent_id))
        self$store$delete_metadata(paste0("opencitations_failed_parent:",
                                          parent_id))
      }

      stats$new_depth_count_end <- self$store$count_frontier_depth(target_depth)
      stats
    },

    store_citations = function(parent_id, citations, target_depth) {
      parent_doi <- doi_from_node_id(parent_id)

      for (citation in citations) {
        source_id <- doi_node_id(citation$citing_doi)
        if (is.na(source_id)) source_id <- pmid_node_id(citation$citing_pmid)
        if (is.na(source_id)) next

        is_retr <- is_seed_id(self$store, source_id)

        self$store$upsert_work(list(
          openalex_id = source_id,
          doi = clean_doi(citation$citing_doi),
          title = NA_character_,
          publication_date = citation$creation_date,
          publication_year = `_year`(citation$creation_date),
          work_type = NA_character_,
          is_retracted = is_retr,
          cited_by_count = NA_integer_,
          source_id = NA_character_,
          source_name = NA_character_,
          topic_id = NA_character_,
          topic_name = NA_character_,
          topic_domain = NA_character_,
          referenced_works_json = json_dumps(list(parent_id)),
          raw_json = json_dumps(citation$raw)
        ))

        self$store$add_frontier_node(source_id, target_depth)

        target_edge <- doi_node_id(citation$cited_doi)
        if (is.na(target_edge)) target_edge <- parent_id

        self$store$add_edge(source_id, target_edge, target_depth,
                           source_api = "opencitations",
                           citation_date = citation$creation_date)

        if (!is.na(parent_doi) && !is_missing(citation$cited_doi) &&
            !is.na(clean_doi(citation$cited_doi)) &&
            clean_doi(citation$cited_doi) != parent_doi) {
          self$store$add_edge(source_id, parent_id, target_depth,
                              source_api = "opencitations",
                              citation_date = citation$creation_date)
        }
      }
      invisible(NULL)
    }
  )
)
