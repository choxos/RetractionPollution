library(testthat)

root <- file.path(testthat::test_path("..", ".."))
for (f in c("R/util.R", "R/seed_units.R", "R/storage.R", "R/analysis.R",
            "R/validation.R", "R/report.R")) {
  source_path <- file.path(root, f)
  if (file.exists(source_path)) {
    source(source_path)
  }
}

test_that("write_report creates a report and final artifact validation", {
  # Given: a minimal analyzed study store.
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- StudyStore$new(db)
  on.exit(store$close(), add = TRUE)
  out <- withr::local_tempdir()

  store$set_metadata("pipeline_mode", "opencitations")
  store$set_metadata("rw_snapshot_date", "20260101T000000Z")
  store$set_metadata("oc_access_date", "2026-01-02")
  store$set_metadata("openalex_access_date", "not used")
  store$set_metadata("last_crawl_summary", list(levels = list()))
  store$set_metadata("depth3_truncated", "false")

  store$upsert_seed(list(
    record_id = "R1", title = "Seed", notice_type = "Retraction",
    notice_date = as.Date("2020-01-01"),
    original_paper_date = as.Date("2019-01-01"),
    original_doi = "10.seed/a", original_pmid = NA,
    author = "A", journal = "J", publisher = "P", subject = "S",
    reason = "R", article_type = "Article", country = "US",
    openalex_id = "doi:10.seed/a", resolved_by = "test",
    resolved_status = "resolved", source_row_json = NA
  ))
  add_work <- function(id, title, pub_date, depth) {
    store$upsert_work(list(
      openalex_id = id, doi = NA, title = title,
      publication_date = as.Date(pub_date),
      publication_year = as.integer(substr(pub_date, 1, 4)),
      work_type = "Article", is_retracted = depth == 0L,
      cited_by_count = 0L, source_id = NA, source_name = "Journal",
      topic_id = NA, topic_name = NA, topic_domain = NA,
      referenced_works_json = "[]", raw_json = NA
    ))
    store$add_frontier_node(id, depth)
  }
  add_work("doi:10.seed/a", "Seed", "2019-01-01", 0L)
  add_work("doi:10.citer/b", "Citer", "2021-01-01", 1L)
  store$add_edge("doi:10.citer/b", "doi:10.seed/a", 1L,
                 source_api = "opencitations",
                 citation_date = as.Date("2021-01-01"))

  # When: analysis and report generation run through the public helpers.
  run_analysis(store, out, max_analysis_depth = 2L)
  report_path <- write_report(store, out)

  # Then: the report and validation table exist and validation sees the report.
  expect_true(file.exists(file.path(out, "report.md")))
  expect_true(file.exists(file.path(out, "tables", "artifact_validation.csv")))
  expect_equal(basename(report_path) %in% c("report.md", "report.html"), TRUE)

  validation <- readr::read_csv(
    file.path(out, "tables", "artifact_validation.csv"),
    show_col_types = FALSE
  )
  report_check <- validation[validation$check == "report_exists", ]
  expect_equal(report_check$status, "pass")
})

test_that("mark_duplicate_seed_resolutions collapses duplicate DOI seeds", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- StudyStore$new(db)
  on.exit(store$close(), add = TRUE)

  # Two seeds with the same DOI but different record IDs and notice dates.
  store$upsert_seed(list(
    record_id = "R1", title = "Paper A", notice_type = "Expression of concern",
    notice_date = as.Date("2020-01-01"), original_paper_date = as.Date("2019-01-01"),
    original_doi = "10.dup/a", original_pmid = NA, author = NA, journal = NA,
    publisher = NA, subject = NA, reason = NA, article_type = NA, country = NA,
    openalex_id = "doi:10.dup/a", resolved_by = "test",
    resolved_status = "resolved", source_row_json = NA
  ))
  store$upsert_seed(list(
    record_id = "R2", title = "Paper A (retraction)", notice_type = "Retraction",
    notice_date = as.Date("2021-06-01"), original_paper_date = as.Date("2019-01-01"),
    original_doi = "10.dup/a", original_pmid = NA, author = NA, journal = NA,
    publisher = NA, subject = NA, reason = NA, article_type = NA, country = NA,
    openalex_id = "doi:10.dup/a", resolved_by = "test",
    resolved_status = "resolved", source_row_json = NA
  ))

  # Before: both are "resolved".
  before <- DBI::dbGetQuery(store$con,
    "SELECT record_id, resolved_status FROM seeds ORDER BY record_id")
  expect_equal(before$resolved_status, c("resolved", "resolved"))

  # Mark duplicates.
  dup_count <- mark_duplicate_seed_resolutions(store)
  expect_equal(dup_count, 1L)

  # After: earliest (R1, 2020-01-01) stays "resolved", R2 becomes "duplicate_doi".
  after <- DBI::dbGetQuery(store$con,
    "SELECT record_id, resolved_status, notice_date FROM seeds ORDER BY record_id")
  expect_equal(after$resolved_status, c("resolved", "duplicate_doi"))
})

test_that("validate_study_outputs fails on missing metadata and unprocessed frontier", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- StudyStore$new(db)
  on.exit(store$close(), add = TRUE)
  out <- withr::local_tempdir()

  # Minimal store with no metadata and an unprocessed frontier node.
  store$upsert_seed(list(
    record_id = "R1", title = "Seed", notice_type = "Retraction",
    notice_date = as.Date("2020-01-01"), original_paper_date = as.Date("2019-01-01"),
    original_doi = "10.seed/a", original_pmid = NA, author = NA, journal = NA,
    publisher = NA, subject = NA, reason = NA, article_type = NA, country = NA,
    openalex_id = "doi:10.seed/a", resolved_by = "test",
    resolved_status = "resolved", source_row_json = NA
  ))
  store$add_frontier_node("doi:10.seed/a", 0L)
  # Do NOT mark processed — frontier_processed check should fail.

  v <- validate_study_outputs(store, out, max_analysis_depth = 2L,
                               include_report = FALSE)

  # Required metadata should fail (no keys set).
  meta_check <- v[v$check == "required_metadata", ]
  expect_equal(meta_check$status, "fail")

  # Frontier processed should fail (depth-0 node is unprocessed).
  frontier_check <- v[v$check == "frontier_processed_for_depth2_claim", ]
  expect_equal(frontier_check$status, "fail")
})

test_that("validate_study_outputs passes on a clean complete store", {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- StudyStore$new(db)
  on.exit(store$close(), add = TRUE)
  out <- withr::local_tempdir()

  # Set all required metadata.
  store$set_metadata("pipeline_mode", "opencitations")
  store$set_metadata("rw_snapshot_date", "20260101T000000Z")
  store$set_metadata("oc_access_date", "2026-01-02")
  store$set_metadata("openalex_access_date", "not used")
  store$set_metadata("last_crawl_summary", list(levels = list()))
  store$set_metadata("depth3_truncated", "false")
  store$set_metadata("opencitations_seed_stats", list(doi_seeds = 1L))

  # One seed, fully processed.
  store$upsert_seed(list(
    record_id = "R1", title = "Seed", notice_type = "Retraction",
    notice_date = as.Date("2020-01-01"), original_paper_date = as.Date("2019-01-01"),
    original_doi = "10.seed/a", original_pmid = NA, author = NA, journal = NA,
    publisher = NA, subject = NA, reason = NA, article_type = NA, country = NA,
    openalex_id = "doi:10.seed/a", resolved_by = "test",
    resolved_status = "resolved", source_row_json = NA
  ))
  store$add_frontier_node("doi:10.seed/a", 0L)
  store$mark_processed(list("doi:10.seed/a"))

  v <- validate_study_outputs(store, out, max_analysis_depth = 2L,
                               include_report = FALSE)

  meta_check <- v[v$check == "required_metadata", ]
  expect_equal(meta_check$status, "pass")

  frontier_check <- v[v$check == "frontier_processed_for_depth2_claim", ]
  expect_equal(frontier_check$status, "pass")

  dup_check <- v[v$check == "duplicate_canonical_seed_records", ]
  expect_equal(dup_check$status, "pass")
})
