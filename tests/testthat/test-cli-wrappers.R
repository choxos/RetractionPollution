library(testthat)

root <- file.path(testthat::test_path("..", ".."))
for (f in c("R/util.R", "R/config.R", "R/cli.R")) {
  source_path <- file.path(root, f)
  if (file.exists(source_path)) {
    source(source_path)
  }
}

test_that("README run wrappers are available", {
  expect_true(exists("run_all", mode = "function"))
  expect_true(exists("run_opencitations", mode = "function"))
  expect_true("data_dir" %in% names(formals(run_all)))
  expect_true("data_dir" %in% names(formals(run_opencitations)))
})
