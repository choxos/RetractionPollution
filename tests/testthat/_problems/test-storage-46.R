# Extracted from test-storage.R:46

# prequel ----------------------------------------------------------------------
library(testthat)
util_path <- file.path(testthat::test_path("..", "..", "R", "util.R"))
if (file.exists(util_path)) source(util_path)
storage_path <- file.path(testthat::test_path("..", "..", "R", "storage.R"))
if (file.exists(storage_path)) source(storage_path)
new_store <- function(db_path) {
  store <- StudyStore$new(db_path)
  withr::defer(store$close())
  store
}

# test -------------------------------------------------------------------------
db <- withr::local_tempfile(fileext = ".duckdb")
store <- new_store(db)
seed <- list(
    record_id = "R1", title = "Initial Title",
    notice_type = "Retraction",
    notice_date = as.Date("2026-01-21"),
    original_paper_date = as.Date("2020-06-01"),
    original_doi = "10.1000/abc123",
    original_pmid = "12345",
    author = "Jane Q. Smith", journal = "Nature",
    publisher = "Springer", subject = "Biology",
    reason = "Falsified data", article_type = "Research",
    country = "USA", openalex_id = "W111",
    resolved_by = NA, resolved_status = "pending",
    source_row_json = NA
  )
store$upsert_seed(seed)
