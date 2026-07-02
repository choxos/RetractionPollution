# Extracted from test-storage.R:85

# prequel ----------------------------------------------------------------------
library(testthat)
util_path <- file.path(testthat::test_path("..", "..", "R", "util.R"))
if (file.exists(util_path)) source(util_path)
storage_path <- file.path(testthat::test_path("..", "..", "R", "storage.R"))
if (file.exists(storage_path)) source(storage_path)
new_store <- function(db_path, env = parent.frame()) {
  store <- StudyStore$new(db_path)
  withr::defer(store$close(), envir = env)
  store
}

# test -------------------------------------------------------------------------
db <- withr::local_tempfile(fileext = ".duckdb")
store <- new_store(db)
seeds <- list(
    list(record_id = "R1", title = "A", source_row_json = NA),
    list(record_id = "R2", title = "B", source_row_json = NA),
    list(record_id = "R3", title = "C", source_row_json = NA)
  )
n <- store$upsert_seeds(seeds)
