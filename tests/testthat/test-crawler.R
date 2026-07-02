library(testthat)

# Source package files directly so tests run without a built package.
for (f in c("util.R", "rw.R", "opencitations.R", "storage.R",
            "openalex.R", "crawler.R")) {
  path <- file.path(testthat::test_path("..", "..", "R", f))
  if (file.exists(path)) source(path)
}

new_store <- function(db_path, env = parent.frame()) {
  store <- StudyStore$new(db_path)
  withr::defer(store$close(), envir = env)
  store
}

# ---------------------------------------------------------------------------
# Fakes
# ---------------------------------------------------------------------------

# Fake OpenAlex client matching the real OpenAlexClient API.
# Methods that should NOT be called raise an error. Construct with specific
# return values to exercise a particular code path.
FakeOpenAlexClient <- R6::R6Class(
  "FakeOpenAlexClient",
  cloneable = FALSE,
  public = list(
    works_by_doi = NULL,
    work_by_pmid_value = NULL,
    search_value = NULL,
    citers_pages = NULL,
    call_log = NULL,

    initialize = function(works_by_doi = list(), work_by_pmid_value = NULL,
                         search_value = NULL, citers_pages = list()) {
      self$works_by_doi <- works_by_doi
      self$work_by_pmid_value <- work_by_pmid_value
      self$search_value <- search_value
      self$citers_pages <- citers_pages
      self$call_log <- list()
    },

    get_works_by_dois = function(dois) {
      if (is.null(self$works_by_doi)) {
        stop("should not be called: get_works_by_dois")
      }
      self$call_log <- c(self$call_log, list(list(fn = "get_works_by_dois", dois = dois)))
      self$works_by_doi
    },

    get_work_by_pmid = function(pmid) {
      if (is.null(self$work_by_pmid_value)) {
        stop("should not be called: get_work_by_pmid")
      }
      self$call_log <- c(self$call_log, list(list(fn = "get_work_by_pmid", pmid = pmid)))
      self$work_by_pmid_value
    },

    search_work = function(title, author_last_name = NULL) {
      if (is.null(self$search_value)) {
        stop("should not be called: search_work")
      }
      self$call_log <- c(self$call_log, list(list(fn = "search_work",
                                                  title = title,
                                                  author_last_name = author_last_name)))
      self$search_value
    },

    list_citers = function(parent_ids, cursor = "*", per_page = 100L) {
      if (length(self$citers_pages) == 0L) {
        stop("should not be called: list_citers")
      }
      page <- self$citers_pages[[1L]]
      self$citers_pages <- self$citers_pages[-1L]
      self$call_log <- c(self$call_log, list(list(fn = "list_citers",
                                                  parent_ids = parent_ids,
                                                  cursor = cursor,
                                                  per_page = per_page)))
      page
    }
  )
)

# Fake OpenCitations client matching the real OpenCitationsClient API.
FakeOpenCitationsClient <- R6::R6Class(
  "FakeOpenCitationsClient",
  cloneable = FALSE,
  public = list(
    citations_map = NULL,

    initialize = function(citations_map = list()) {
      self$citations_map <- citations_map
    },

    citations_by_doi = function(doi) {
      self$citations_map[[doi]] %||% list()
    }
  )
)

# ---------------------------------------------------------------------------
# resolve_pending_seeds
# ---------------------------------------------------------------------------

test_that("resolve_pending_seeds marks no-DOI/PMID seed pending_title_fallback (no network)", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  seed <- list(
    record_id = "S1", title = "No ids", notice_type = "Retraction",
    notice_date = as.Date("2024-01-01"), original_paper_date = as.Date("2020-01-01"),
    original_doi = NA_character_, original_pmid = NA_character_,
    author = "A Smith", journal = NA_character_, publisher = NA_character_,
    subject = NA_character_, reason = NA_character_, article_type = NA_character_,
    country = NA_character_, openalex_id = NA_character_,
    resolved_by = NA_character_, resolved_status = "pending",
    source_row_json = "{}"
  )
  store$upsert_seed(seed)

  client <- FakeOpenAlexClient$new()
  stats <- resolve_pending_seeds(store, client, title_fallback = FALSE)

  expect_equal(stats$pending_title_fallback, 1L)
  expect_equal(stats$resolved, 0L)

  status <- DBI::dbGetQuery(store$con,
    "SELECT resolved_status, resolved_by FROM seeds WHERE record_id='S1'"
  )
  expect_equal(status$resolved_status[1], "pending_title_fallback")
  expect_equal(status$resolved_by[1], "exact")

  # No network calls were made.
  expect_length(client$call_log, 0L)
})

test_that("resolve_pending_seeds marks not_found when title_fallback=TRUE", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  seed <- list(
    record_id = "S2", title = NA_character_, notice_type = "Retraction",
    notice_date = as.Date("2024-01-01"), original_paper_date = as.Date("2020-01-01"),
    original_doi = NA_character_, original_pmid = NA_character_,
    author = NA_character_, journal = NA_character_, publisher = NA_character_,
    subject = NA_character_, reason = NA_character_, article_type = NA_character_,
    country = NA_character_, openalex_id = NA_character_,
    resolved_by = NA_character_, resolved_status = "pending_title_fallback",
    source_row_json = "{}"
  )
  store$upsert_seed(seed)

  # title_fallback=TRUE and title is NA -> search not invoked -> not_found.
  client <- FakeOpenAlexClient$new()
  stats <- resolve_pending_seeds(store, client, title_fallback = TRUE)

  expect_equal(stats$not_found, 1L)
  expect_equal(stats$resolved, 0L)

  status <- DBI::dbGetQuery(store$con,
    "SELECT resolved_status FROM seeds WHERE record_id='S2'"
  )
  expect_equal(status$resolved_status[1], "not_found")
})

test_that("resolve_pending_seeds resolves a DOI seed and upserts the work", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  seed <- list(
    record_id = "S3", title = "Has DOI", notice_type = "Retraction",
    notice_date = as.Date("2024-01-01"), original_paper_date = as.Date("2020-01-01"),
    original_doi = "10.1000/abc123", original_pmid = NA_character_,
    author = NA_character_, journal = NA_character_, publisher = NA_character_,
    subject = NA_character_, reason = NA_character_, article_type = NA_character_,
    country = NA_character_, openalex_id = NA_character_,
    resolved_by = NA_character_, resolved_status = "pending",
    source_row_json = "{}"
  )
  store$upsert_seed(seed)

  work <- list(
    id = "https://openalex.org/W12345",
    doi = "https://doi.org/10.1000/abc123",
    display_name = "A Paper",
    title = "A Paper",
    publication_date = "2020-05-01",
    publication_year = 2020L,
    type = "article",
    is_retracted = FALSE,
    cited_by_count = 7L,
    referenced_works = list(),
    primary_location = list(),
    primary_topic = list()
  )
  client <- FakeOpenAlexClient$new(works_by_doi = list(work))

  stats <- resolve_pending_seeds(store, client, title_fallback = FALSE)

  expect_equal(stats$resolved, 1L)
  expect_equal(stats$checked, 1L)

  row <- DBI::dbGetQuery(store$con,
    "SELECT openalex_id, resolved_by, resolved_status FROM seeds WHERE record_id='S3'"
  )
  expect_equal(row$openalex_id[1], "W12345")
  expect_equal(row$resolved_by[1], "doi")
  expect_equal(row$resolved_status[1], "resolved")

  work_row <- DBI::dbGetQuery(store$con,
    "SELECT openalex_id, doi FROM works WHERE openalex_id='W12345'"
  )
  expect_equal(nrow(work_row), 1L)
  expect_equal(work_row$doi[1], "10.1000/abc123")
})

# ---------------------------------------------------------------------------
# CitationCrawler
# ---------------------------------------------------------------------------

make_raw_work <- function(openalex_id, doi, referenced = list()) {
  list(
    id = paste0("https://openalex.org/", openalex_id),
    doi = if (is.na(doi)) NA_character_ else paste0("https://doi.org/", doi),
    display_name = paste0("Work ", openalex_id),
    title = paste0("Work ", openalex_id),
    publication_date = "2021-01-01",
    publication_year = 2021L,
    type = "article",
    is_retracted = FALSE,
    cited_by_count = 0L,
    referenced_works = vapply(referenced, function(r)
      paste0("https://openalex.org/", r), character(1)),
    primary_location = list(),
    primary_topic = list()
  )
}

test_that("CitationCrawler$crawl creates frontier nodes and edges at the right depths", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  # Seed: a retracted paper with a known OpenAlex ID.
  seed <- list(
    record_id = "C1", title = "Retracted", notice_type = "Retraction",
    notice_date = as.Date("2024-01-01"), original_paper_date = as.Date("2020-01-01"),
    original_doi = "10.2000/seed", original_pmid = NA_character_,
    author = NA_character_, journal = NA_character_, publisher = NA_character_,
    subject = NA_character_, reason = NA_character_, article_type = NA_character_,
    country = NA_character_, openalex_id = "W100",
    resolved_by = "manual", resolved_status = "resolved",
    source_row_json = "{}"
  )
  store$upsert_seed(seed)

  # One citer of W100 that references it (depth 1); empty page at depth 2.
  citer_work <- make_raw_work("W200", "10.3000/citer", referenced = list("W100"))
  page1 <- OpenAlexPage(results = list(citer_work), next_cursor = NULL, count = 1L)
  page2 <- OpenAlexPage(results = list(), next_cursor = NULL, count = 0L)
  openalex <- FakeOpenAlexClient$new(citers_pages = list(page1, page2))

  crawler <- CitationCrawler$new(store, openalex, opencitations = NULL)

  summary <- crawler$crawl(max_depth = 2L, complete_depth = 2L,
                           batch_size = 10L, per_page = 25L)

  # Seed node at depth 0.
  expect_equal(store$count_frontier_depth(0L), 1L)
  # Citer at depth 1 (referenced_works intersects seed parent set).
  expect_gte(store$count_frontier_depth(1L), 1L)

  edges <- DBI::dbGetQuery(store$con,
    "SELECT source_id, target_id, depth, source_api
     FROM citation_edges ORDER BY depth, source_id, target_id"
  )
  expect_gte(nrow(edges), 1L)
  # The citer (W200) cites the seed (W100).
  expect_true(any(edges$source_id == "W200" & edges$target_id == "W100" &
                  edges$depth == 1L))

  # Levels recorded.
  expect_true("1" %in% names(summary$levels))
})

test_that("CitationCrawler$crawl truncates at depth 3 when node cap is hit", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  seed <- list(
    record_id = "C2", title = "Retracted", notice_type = "Retraction",
    notice_date = as.Date("2024-01-01"), original_paper_date = as.Date("2020-01-01"),
    original_doi = "10.2000/seed2", original_pmid = NA_character_,
    author = NA_character_, journal = NA_character_, publisher = NA_character_,
    subject = NA_character_, reason = NA_character_, article_type = NA_character_,
    country = NA_character_, openalex_id = "W500",
    resolved_by = "manual", resolved_status = "resolved",
    source_row_json = "{}"
  )
  store$upsert_seed(seed)

  # Depth 1: one citer of W500.
  d1 <- make_raw_work("W501", "10.3010/d1", referenced = list("W500"))
  page1 <- OpenAlexPage(results = list(d1), next_cursor = NULL, count = 1L)

  # Depth 2: one citer of W501.
  d2 <- make_raw_work("W511", "10.3110/d2", referenced = list("W501"))
  page2 <- OpenAlexPage(results = list(d2), next_cursor = NULL, count = 1L)

  # Depth 3: one citer of W511. The page has a next_cursor so the inner loop
  # re-checks the depth cap (now reached: 1 node >= cap 1) before fetching
  # the next page, triggering truncation.
  d3 <- make_raw_work("W521", "10.3210/d3", referenced = list("W511"))
  page3 <- OpenAlexPage(results = list(d3), next_cursor = "more", count = 2L)

  openalex <- FakeOpenAlexClient$new(citers_pages = list(page1, page2, page3))

  crawler <- CitationCrawler$new(store, openalex, opencitations = NULL)
  summary <- crawler$crawl(max_depth = 3L, complete_depth = 2L,
                           batch_size = 10L, per_page = 25L,
                           depth3_node_cap = 1L, depth3_page_cap = 2500L)

  # A level 3 entry exists and signals truncation.
  level3 <- summary$levels[["3"]]
  expect_false(is.null(level3))
  expect_true(isTRUE(level3$truncated) ||
              (is.list(level3$opencitations) &&
               isTRUE(level3$opencitations$truncated)))
})