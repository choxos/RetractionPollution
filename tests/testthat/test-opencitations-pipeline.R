library(testthat)

# Source dependencies directly so tests run without a built package.
root <- file.path(testthat::test_path("..", ".."))
for (f in c("R/util.R", "R/rw.R", "R/seed_units.R", "R/opencitations.R",
            "R/storage.R", "R/opencitations_pipeline.R")) {
  source_path <- file.path(root, f)
  if (file.exists(source_path)) {
    source(source_path)
  }
}

new_store <- function(db_path, env = parent.frame()) {
  store <- StudyStore$new(db_path)
  withr::defer(store$close(), envir = env)
  store
}

#' Fake OpenCitations client with hardcoded citation maps.
#' Records all calls in $calls.
FakeOpenCitationsClient <- function() {
  calls <- list()

  citations_by_doi <- function(doi) {
    calls[[length(calls) + 1L]] <<- doi
    cleaned <- clean_doi(doi)
    if (!is.na(cleaned) && cleaned == "10.seed/a") {
      list(
        OpenCitation(
          citing_doi = "10.citer/b", cited_doi = "10.seed/a",
          creation_date = "2021-01-01", raw = list(citing = "doi:10.citer/b")
        ),
        OpenCitation(
          citing_doi = "10.citer/c", cited_doi = "10.seed/a",
          creation_date = "2021-02-02", raw = list(citing = "doi:10.citer/c")
        )
      )
    } else if (!is.na(cleaned) && cleaned == "10.citer/b") {
      list(
        OpenCitation(
          citing_doi = "10.depth2/d", cited_doi = "10.citer/b",
          creation_date = "2022-01-01", raw = list(citing = "doi:10.depth2/d")
        )
      )
    } else {
      list()
    }
  }

  structure(
    list(citations_by_doi = citations_by_doi, calls = calls),
    class = "FakeOpenCitationsClient"
  )
}

make_seed <- function(record_id = "S1", doi = "10.seed/a",
                      notice_date = "2026-01-01") {
  list(
    record_id = record_id, title = paste("Title", record_id),
    notice_type = "Retraction", notice_date = notice_date,
    original_paper_date = "2020-06-01", original_doi = doi,
    original_pmid = "12345", author = "Jane Smith", journal = "Nature",
    publisher = "Springer", subject = "Biology", reason = "error",
    article_type = "article", country = "US", openalex_id = NA_character_,
    resolved_by = NA_character_, resolved_status = "pending",
    source_row_json = json_dumps(list(record_id = record_id))
  )
}

edges_df <- function(store) {
  DBI::dbGetQuery(store$con, "
    SELECT source_id, target_id, depth, source_api, citation_date
    FROM citation_edges ORDER BY source_id, target_id, depth
  ")
}

frontier_df <- function(store) {
  DBI::dbGetQuery(store$con, "
    SELECT openalex_id, depth, processed_at
    FROM frontier_nodes ORDER BY depth, openalex_id
  ")
}

works_df <- function(store) {
  DBI::dbGetQuery(store$con, "
    SELECT openalex_id, doi, title, is_retracted, publication_year
    FROM works ORDER BY openalex_id
  ")
}

test_that("prepare_opencitations_seeds loads and resolves a DOI seed", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  stats <- prepare_opencitations_seeds(store, list(make_seed()))

  expect_equal(stats$loaded, 1L)
  expect_equal(stats$doi_seeds, 1L)
  expect_equal(stats$no_doi, 0L)

  seeds <- DBI::dbGetQuery(store$con, "SELECT * FROM seeds")
  expect_equal(seeds$resolved_status[1], "resolved")
  expect_equal(seeds$openalex_id[1], "doi:10.seed/a")
  expect_equal(seeds$resolved_by[1], "opencitations_doi")

  works <- works_df(store)
  expect_equal(nrow(works), 1L)
  expect_true(works$is_retracted[1])
  expect_equal(works$openalex_id[1], "doi:10.seed/a")
  expect_equal(works$publication_year[1], 2020L)
})

test_that("prepare_opencitations_seeds marks duplicate DOI records before frontier construction", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  stats <- prepare_opencitations_seeds(store, list(
    make_seed(record_id = "S1", doi = "10.seed/a",
              notice_date = "2026-01-01"),
    make_seed(record_id = "S1B", doi = "10.seed/a",
              notice_date = "2025-01-01")
  ))

  expect_equal(stats$loaded, 2L)
  expect_equal(stats$doi_seeds, 1L)
  expect_equal(stats$duplicate_doi, 1L)
  expect_equal(store$count_frontier_depth(0L), 1L)

  seeds <- DBI::dbGetQuery(
    store$con,
    "SELECT record_id, openalex_id, resolved_status
     FROM seeds ORDER BY record_id"
  )
  expect_equal(seeds$resolved_status[seeds$record_id == "S1B"], "resolved")
  expect_equal(seeds$resolved_status[seeds$record_id == "S1"], "duplicate_doi")
  expect_equal(seeds$openalex_id[seeds$record_id == "S1"], "doi:10.seed/a")
})

test_that("no_doi seed path marks resolved_status='no_doi'", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  seed <- make_seed(record_id = "S2", doi = NA_character_)
  stats <- prepare_opencitations_seeds(store, list(seed))
  expect_equal(stats$no_doi, 1L)
  expect_equal(stats$doi_seeds, 0L)
  seeds <- DBI::dbGetQuery(store$con, "SELECT * FROM seeds")
  expect_equal(seeds$resolved_status[1], "no_doi")
})

test_that("crawler produces exact edges and frontier depths", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  prepare_opencitations_seeds(store, list(make_seed()))
  client <- FakeOpenCitationsClient()
  crawler <- OpenCitationsOnlyCrawler$new(store, client)
  summary <- crawler$crawl(max_depth = 2L, complete_depth = 2L)

  edges <- edges_df(store)
  expect_equal(nrow(edges), 3L)
  expect_true(all(edges$source_api == "opencitations"))

  # Check exact edge tuples
  tuples <- paste(edges$source_id, edges$target_id, edges$depth, sep = "|")
  expect_true("doi:10.citer/b|doi:10.seed/a|1" %in% tuples)
  expect_true("doi:10.citer/c|doi:10.seed/a|1" %in% tuples)
  expect_true("doi:10.depth2/d|doi:10.citer/b|2" %in% tuples)

  # Frontier depths
  fr <- frontier_df(store)
  expect_equal(fr$depth[fr$openalex_id == "doi:10.seed/a"], 0L)
  expect_equal(fr$depth[fr$openalex_id == "doi:10.citer/b"], 1L)
  expect_equal(fr$depth[fr$openalex_id == "doi:10.citer/c"], 1L)
  expect_equal(fr$depth[fr$openalex_id == "doi:10.depth2/d"], 2L)

  # Seed is_retracted preserved
  wrks <- works_df(store)
  expect_true(wrks$is_retracted[wrks$openalex_id == "doi:10.seed/a"])

  # Summary structure
  expect_equal(summary$mode, "opencitations")
  expect_equal(summary$seed_nodes, 1L)
  expect_equal(summary$max_depth, 2L)
})

test_that("dual-edge branch creates two edges when cited_doi != parent_doi", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  prepare_opencitations_seeds(store, list(make_seed()))

  # Custom client returning a citation whose cited_doi differs from parent_doi
  client <- list(
    citations_by_doi = function(doi) {
      if (clean_doi(doi) == "10.seed/a") {
        list(OpenCitation(
          citing_doi = "10.citer/x", cited_doi = "10.other/y",
          creation_date = "2021-03-03", raw = list(citing = "doi:10.citer/x")
        ))
      } else {
        list()
      }
    }
  )
  crawler <- OpenCitationsOnlyCrawler$new(store, client)
  crawler$crawl(max_depth = 1L, complete_depth = 1L)

  edges <- edges_df(store)
  # Expect edges to BOTH doi:10.other/y AND doi:10.seed/a at depth 1
  src <- edges[edges$source_id == "doi:10.citer/x", ]
  expect_true("doi:10.other/y" %in% src$target_id)
  expect_true("doi:10.seed/a" %in% src$target_id)
})

test_that("successful retry clears stale failed-parent metadata", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  prepare_opencitations_seeds(store, list(make_seed()))
  store$set_metadata("opencitations_failed_parent:doi:10.seed/a",
                     "old failure")

  client <- list(citations_by_doi = function(doi) list())
  crawler <- OpenCitationsOnlyCrawler$new(store, client)
  crawler$crawl(max_depth = 1L, complete_depth = 1L)

  failed <- DBI::dbGetQuery(
    store$con,
    "SELECT COUNT(*) AS n
     FROM run_metadata
     WHERE key = 'opencitations_failed_parent:doi:10.seed/a'"
  )
  expect_equal(failed$n[1], 0)
})

test_that("depth-3 truncation triggers truncated=TRUE under node cap", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  prepare_opencitations_seeds(store, list(make_seed()))
  client <- FakeOpenCitationsClient()
  crawler <- OpenCitationsOnlyCrawler$new(store, client)
  # complete_depth=1 makes depth-2 capped; cap=1 should truncate immediately
  # at the start of processing the first parent at depth 1.
  summary <- crawler$crawl(max_depth = 2L, complete_depth = 1L,
                            depth3_node_cap = 1L)
  # depth 2 (target_depth=2) is capped since 2 > complete_depth(1)
  lvl2 <- summary$levels[["2"]]
  expect_true(isTRUE(lvl2$truncated))
  expect_true(tolower(store$get_metadata("depth2_truncated", "false")) == "true")
})
