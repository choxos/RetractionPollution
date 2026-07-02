# Extracted from test-storage.R:227

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
store$upsert_seed(list(record_id = "R1", title = "A",
                         openalex_id = "W1", source_row_json = NA))
