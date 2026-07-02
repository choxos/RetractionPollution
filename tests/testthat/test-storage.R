library(testthat)

# Source util.R and storage.R directly so tests run without a built package.
util_path <- file.path(testthat::test_path("..", "..", "R", "util.R"))
if (file.exists(util_path)) source(util_path)
storage_path <- file.path(testthat::test_path("..", "..", "R", "storage.R"))
if (file.exists(storage_path)) source(storage_path)

new_store <- function(db_path, env = parent.frame()) {
  store <- StudyStore$new(db_path)
  withr::defer(store$close(), envir = env)
  store
}

test_that("StudyStore$initialize creates all six tables", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  df <- DBI::dbGetQuery(
    store$con,
    "SELECT table_name FROM information_schema.tables
     WHERE table_schema = 'main' ORDER BY table_name"
  )
  expected <- c("citation_edges", "crawl_jobs", "frontier_nodes",
                "run_metadata", "seeds", "works")
  expect_setequal(df$table_name, expected)
})

test_that("upsert_seed inserts and updates a seed row", {
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

  got <- DBI::dbGetQuery(
    store$con,
    "SELECT record_id, title, openalex_id FROM seeds WHERE record_id = ?",
    params = list("R1")
  )
  expect_equal(nrow(got), 1L)
  expect_equal(got$title, "Initial Title")
  expect_equal(got$openalex_id, "W111")

  # Frontier node should be seeded at depth 0.
  expect_equal(store$count_frontier_depth(0L), 1L)

  # Re-insert with a new title; verify update + timestamp bump.
  seed2 <- seed
  seed2$title <- "Updated Title"
  before <- DBI::dbGetQuery(
    store$con,
    "SELECT updated_at FROM seeds WHERE record_id = 'R1'"
  )$updated_at
  Sys.sleep(1.1)
  store$upsert_seed(seed2)
  after <- DBI::dbGetQuery(
    store$con,
    "SELECT title, updated_at FROM seeds WHERE record_id = 'R1'"
  )
  expect_equal(after$title, "Updated Title")
  expect_true(after$updated_at > before)
})

test_that("upsert_seeds inserts multiple and returns the count", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)
  seeds <- list(
    list(record_id = "R1", title = "A", source_row_json = NA),
    list(record_id = "R2", title = "B", source_row_json = NA),
    list(record_id = "R3", title = "C", source_row_json = NA)
  )
  n <- store$upsert_seeds(seeds)
  expect_equal(n, 3L)
  ids <- DBI::dbGetQuery(
    store$con,
    "SELECT record_id FROM seeds ORDER BY record_id"
  )$record_id
  expect_equal(ids, c("R1", "R2", "R3"))
})

test_that("add_frontier_node promotes to a shallower depth (BFS)", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  store$add_frontier_node("W1", 2L)
  store$add_frontier_node("W1", 1L)  # shallower -> promote
  store$add_frontier_node("W2", 0L)  # new node, stays 0

  df <- DBI::dbGetQuery(
    store$con,
    "SELECT openalex_id, depth FROM frontier_nodes ORDER BY openalex_id"
  )
  expect_equal(df$depth[df$openalex_id == "W1"], 1L)
  expect_equal(df$depth[df$openalex_id == "W2"], 0L)

  # Adding a deeper depth does NOT promote.
  store$add_frontier_node("W1", 5L)
  d <- DBI::dbGetQuery(
    store$con,
    "SELECT depth FROM frontier_nodes WHERE openalex_id = 'W1'"
  )$depth[1]
  expect_equal(d, 1L)
})

test_that("add_edge min-depth merge keeps the shallowest (HIGH-2)", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  # Insert edge A->B at depth 2 from openalex.
  store$add_edge("A", "B", 2L, source_api = "openalex")

  # Re-observe at depth 1 from opencitations -> shallower wins.
  store$add_edge("A", "B", 1L, source_api = "opencitations")
  row <- DBI::dbGetQuery(
    store$con,
    "SELECT depth, source_api FROM citation_edges
     WHERE source_id = 'A' AND target_id = 'B'"
  )
  expect_equal(row$depth[1], 1L)
  expect_equal(row$source_api[1], "opencitations")

  # Re-observe at depth 3 -> min preserved at 1, api unchanged.
  store$add_edge("A", "B", 3L, source_api = "openalex")
  row2 <- DBI::dbGetQuery(
    store$con,
    "SELECT depth, source_api FROM citation_edges
     WHERE source_id = 'A' AND target_id = 'B'"
  )
  expect_equal(row2$depth[1], 1L)
  expect_equal(row2$source_api[1], "opencitations")
})

test_that("set_metadata/get_metadata round-trips string and list values", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  store$set_metadata("k_str", "hello")
  expect_equal(store$get_metadata("k_str"), "hello")

  store$set_metadata("k_list", list(a = 1, b = "x"))
  got <- store$get_metadata("k_list")
  expect_type(got, "character")
  expect_true(jsonlite::validate(got))

  # Default when missing.
  expect_equal(store$get_metadata("missing", "fallback"), "fallback")
})

test_that("count_frontier_depth counts nodes per depth", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  store$add_frontier_node("W1", 0L)
  store$add_frontier_node("W2", 0L)
  store$add_frontier_node("W3", 1L)
  store$add_frontier_node("W4", 1L)
  store$add_frontier_node("W5", 1L)

  expect_equal(store$count_frontier_depth(0L), 2L)
  expect_equal(store$count_frontier_depth(1L), 3L)
  expect_equal(store$count_frontier_depth(2L), 0L)
})

test_that("pending_frontier returns only unprocessed nodes", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  store$add_frontier_node("W1", 1L)
  store$add_frontier_node("W2", 1L)
  store$add_frontier_node("W3", 1L)

  store$mark_processed(c("W2"))

  pending <- store$pending_frontier(1L)
  expect_setequal(pending, c("W1", "W3"))

  # Limit works.
  expect_length(store$pending_frontier(1L, limit = 1L), 1L)

  # Different depth returns nothing.
  expect_length(store$pending_frontier(2L), 0L)
})

test_that("get_or_create_job / get_job / update_job round-trip", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  job <- store$get_or_create_job("job1", 2L, c("W1", "W2"))
  expect_equal(job$job_id, "job1")
  expect_equal(job$depth, 2L)
  expect_equal(job$cursor, "*")
  expect_false(job$done)

  # Idempotent create.
  job2 <- store$get_or_create_job("job1", 2L, c("W1", "W2"))
  expect_equal(job2$job_id, "job1")

  store$update_job("job1", cursor = "nextpage", done = TRUE,
                   pages_delta = 2L, results_delta = 15L)
  job3 <- store$get_job("job1")
  expect_equal(job3$cursor, "nextpage")
  expect_true(job3$done)
  expect_equal(job3$pages_fetched, 2L)
  expect_equal(job3$results_fetched, 15L)

  expect_error(store$get_job("nope"), "job not found")
})

test_that("resolved_seed_ids and unresolved_seeds filter correctly", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- new_store(db)

  store$upsert_seed(list(record_id = "R1", title = "A",
                         openalex_id = "W1", source_row_json = NA))
  store$upsert_seed(list(record_id = "R2", title = "B",
                         openalex_id = NA, resolved_status = "pending",
                         source_row_json = NA))
  store$upsert_seed(list(record_id = "R3", title = "C",
                         openalex_id = NA, resolved_status = "not_found",
                         source_row_json = NA))

  ids <- store$resolved_seed_ids()
  expect_equal(ids, "W1")

  unresolved <- store$unresolved_seeds()
  rec_ids <- vapply(unresolved, function(r) r$record_id, character(1))
  # R2 is pending (included), R3 is not_found (excluded).
  expect_setequal(rec_ids, "R2")

  # Limit works.
  expect_length(store$unresolved_seeds(limit = 0L), 0L)
})