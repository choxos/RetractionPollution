# Extracted from test-storage.R:201

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
job <- store$get_or_create_job("job1", 2L, c("W1", "W2"))
