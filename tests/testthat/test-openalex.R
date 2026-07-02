library(testthat)

# Source util.R and openalex.R directly so tests run without a built package.
util_path <- file.path(testthat::test_path("..", "..", "R", "util.R"))
if (file.exists(util_path)) source(util_path)
openalex_path <- file.path(testthat::test_path("..", "..", "R", "openalex.R"))
if (file.exists(openalex_path)) source(openalex_path)

sample_work <- function() {
  list(
    id = "https://openalex.org/W1",
    doi = "https://doi.org/10.1/ABC",
    display_name = "A work",
    title = "A work (full)",
    publication_date = "2024-01-01",
    publication_year = 2024L,
    type = "article",
    type_crossref = "journal-article",
    is_retracted = FALSE,
    cited_by_count = 5L,
    referenced_works = list("https://openalex.org/W0", "https://openalex.org/W11"),
    primary_location = list(
      source = list(
        id = "https://openalex.org/S1",
        display_name = "Journal of Tests"
      )
    ),
    primary_topic = list(
      id = "https://openalex.org/T1",
      display_name = "Topic Name",
      domain = list(display_name = "Health sciences")
    )
  )
}

test_that("normalize_work extracts core metadata", {
  row <- normalize_work(sample_work())

  expect_equal(row$openalex_id, "W1")
  expect_equal(row$doi, "10.1/abc")
  expect_equal(row$title, "A work")
  expect_equal(row$source_id, "S1")
  expect_equal(row$source_name, "Journal of Tests")
  expect_equal(row$topic_id, "T1")
  expect_equal(row$topic_name, "Topic Name")
  expect_equal(row$topic_domain, "Health sciences")
  expect_equal(row$publication_year, 2024L)
  expect_false(row$is_retracted)
  expect_equal(row$cited_by_count, 5L)
  expect_true(nzchar(row$referenced_works_json))
  expect_true(nzchar(row$raw_json))
})

test_that("normalize_work referenced_works_json holds compact non-NA ids", {
  row <- normalize_work(sample_work())
  parsed <- jsonlite::fromJSON(row$referenced_works_json)
  expect_setequal(parsed, c("W0", "W11"))
})

test_that("normalize_work falls back to title when display_name missing", {
  work <- sample_work()
  work$display_name <- NULL
  row <- normalize_work(work)
  expect_equal(row$title, "A work (full)")
})

test_that("normalize_work handles missing primary_location and primary_topic", {
  work <- sample_work()
  work$primary_location <- NULL
  work$primary_topic <- NULL
  row <- normalize_work(work)
  expect_true(is.na(row$source_id))
  expect_true(is.na(row$source_name))
  expect_true(is.na(row$topic_id))
  expect_true(is.na(row$topic_name))
  expect_true(is.na(row$topic_domain))
})

test_that("normalize_work handles primary_location with no source", {
  work <- sample_work()
  work$primary_location <- list(source = NULL)
  row <- normalize_work(work)
  expect_true(is.na(row$source_id))
  expect_true(is.na(row$source_name))
})

test_that("normalize_work filters NA/non-nzchar referenced ids", {
  work <- sample_work()
  work$referenced_works <- list(
    "https://openalex.org/W0",
    NA,
    "not-an-id",
    "https://openalex.org/W11"
  )
  row <- normalize_work(work)
  parsed <- jsonlite::fromJSON(row$referenced_works_json)
  expect_setequal(parsed, c("W0", "W11", "not-an-id"))
})

test_that("normalize_work uses type_crossref when type missing", {
  work <- sample_work()
  work$type <- NULL
  row <- normalize_work(work)
  expect_equal(row$work_type, "journal-article")
})

test_that("edges_from_work intersects references with parent set, sorted", {
  work <- list(
    id = "https://openalex.org/W2",
    referenced_works = list(
      "https://openalex.org/W0",
      "https://openalex.org/W9",
      "https://openalex.org/W1"
    )
  )
  edges <- edges_from_work(work, c("W0", "W1"))
  expect_equal(edges, list(c("W2", "W0"), c("W2", "W1")))
})

test_that("edges_from_work returns empty when no intersection", {
  work <- list(
    id = "https://openalex.org/W2",
    referenced_works = list("https://openalex.org/W9")
  )
  edges <- edges_from_work(work, c("W0", "W1"))
  expect_equal(edges, list())
})

test_that("edges_from_work returns empty when referenced_works is NULL", {
  work <- list(id = "https://openalex.org/W2", referenced_works = NULL)
  edges <- edges_from_work(work, c("W0"))
  expect_equal(edges, list())
})

test_that("edges_from_work returns empty when source id is NA", {
  work <- list(id = NULL, referenced_works = list("https://openalex.org/W0"))
  edges <- edges_from_work(work, c("W0"))
  expect_equal(edges, list())
})

test_that("edges_from_work deduplicates referenced_works", {
  work <- list(
    id = "https://openalex.org/W2",
    referenced_works = list("https://openalex.org/W0", "https://openalex.org/W0")
  )
  edges <- edges_from_work(work, c("W0"))
  expect_equal(edges, list(c("W2", "W0")))
})

test_that("OpenAlexClient initializes with defaults", {
  client <- OpenAlexClient$new()
  expect_equal(client$base_url, OPENALEX_API)
  expect_equal(client$retries, 6L)
  expect_equal(client$request_delay, 0.35)
  expect_equal(client$rate_limit_sleep, 60.0)
  expect_true(is.na(client$api_key))
  expect_true(is.na(client$email))
})

test_that("OpenAlexClient initializes with api_key and email", {
  client <- OpenAlexClient$new(api_key = "k", email = "u@x.org")
  expect_equal(client$api_key, "k")
  expect_equal(client$email, "u@x.org")
})

test_that("OpenAlexClient strips trailing slash from base_url", {
  client <- OpenAlexClient$new(base_url = "https://api.openalex.org/")
  expect_equal(client$base_url, "https://api.openalex.org")
})

# FakeOpenAlexClient mirrors FailingBatchOpenAlexClient from the Python tests.
FakeOpenAlexClient <- R6::R6Class(
  "FakeOpenAlexClient",
  inherit = OpenAlexClient,
  cloneable = FALSE,
  public = list(
    filters = list(),
    initialize = function() {
      super$initialize(api_key = "test", request_delay = 0)
      self$filters <- list()
    }
  ),
  private = list(
    request_json = function(path, params = list()) {
      self$filters <- c(self$filters, list(params$filter))
      filter_value <- params$filter
      if (!is.null(filter_value) && grepl("|", filter_value, fixed = TRUE)) {
        openalex_error("bad batch", status_code = 400L)
      }
      if (!is.null(filter_value) && grepl("10.1234/good", filter_value, fixed = TRUE)) {
        return(list(results = list(list(
          id = "https://openalex.org/W1",
          doi = "https://doi.org/10.1234/good"
        ))))
      }
      openalex_error("bad singleton", status_code = 400L)
    }
  )
)

test_that("get_works_by_dois bisects bad batches and skips bad singletons", {
  client <- FakeOpenAlexClient$new()
  works <- client$get_works_by_dois(c("10.1234/good", "10.1234/bad"))
  expect_equal(vapply(works, function(w) w$id, character(1)),
               "https://openalex.org/W1")
  expect_equal(length(client$filters), 3L)
})

test_that("get_works_by_dois returns empty for no DOIs", {
  client <- FakeOpenAlexClient$new()
  expect_equal(client$get_works_by_dois(character(0)), list())
  expect_equal(client$get_works_by_dois(NA_character_), list())
})

test_that("list_citers errors when more than 100 parent_ids", {
  client <- OpenAlexClient$new(request_delay = 0)
  ids <- paste0("W", seq_len(101L))
  expect_error(client$list_citers(ids), "at most 100")
})

test_that("list_citers returns empty page when no ids", {
  client <- OpenAlexClient$new(request_delay = 0)
  page <- client$list_citers(character(0))
  expect_equal(page$results, list())
  expect_equal(page$count, 0L)
})

test_that("OpenAlexError and OpenAlexRateLimitError are raisable conditions", {
  expect_error(openalex_error("boom", status_code = 500L),
               class = "OpenAlexError")
  expect_error(openalex_rate_limit_error("slow"),
               class = "OpenAlexRateLimitError")
  cond <- tryCatch(
    openalex_rate_limit_error("slow", status_code = 429L),
    OpenAlexRateLimitError = function(c) c
  )
  expect_s3_class(cond, "OpenAlexError")
  expect_equal(cond$status_code, 429L)
})