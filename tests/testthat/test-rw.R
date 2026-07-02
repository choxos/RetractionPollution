test_that("is_seed_notice flags retractions and EOCs", {
  expect_true(is_seed_notice("Retraction"))
  expect_true(is_seed_notice("Expression of concern"))
  expect_true(is_seed_notice("Correction; Retraction"))
  expect_false(is_seed_notice("Correction"))
  expect_false(is_seed_notice(NA))
  expect_false(is_seed_notice(NA_character_))
})

test_that("load_seed_rows filters and normalizes end-to-end", {
  csv <- withr::local_tempfile(fileext = ".csv")
  df <- data.frame(
    `Record ID` = c("R1", "R2", "R3"),
    Title = c("A retracted paper", "A correction", "An EOC paper"),
    RetractionNature = c("Retraction", "Correction", "Expression of concern"),
    RetractionDate = c("1/21/2026 0:00", "2/14/2025 0:00", "3/7/2026 0:00"),
    OriginalPaperDate = c("6/1/2020 0:00", "5/1/2019 0:00", "9/1/2018 0:00"),
    OriginalPaperDOI = c("https://doi.org/10.1000/ABC123", "Unavailable", "10.2000/xyz-789"),
    OriginalPaperPubMedID = c("12345", "0", "67890"),
    Author = c("Jane Q. Smith; Bob Lee", "Solo Author", "Carol A. Johnson"),
    Journal = c("Nature", "Science", "Cell"),
    Publisher = c("Springer", "Elsevier", "Wiley"),
    Subject = c("Biology", "Physics", "Chemistry"),
    Reason = c("Falsified data", "Error", "Concern"),
    ArticleType = c("Research", "Research", "Review"),
    Country = c("USA", "UK", "Canada"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, csv)

  seeds <- load_seed_rows(csv)
  expect_length(seeds, 2)

  expect_equal(seeds[[1]]$record_id, "R1")
  expect_equal(seeds[[1]]$notice_type, "Retraction")
  expect_equal(seeds[[1]]$notice_date, "2026-01-21")
  expect_equal(seeds[[1]]$original_doi, "10.1000/abc123")
  expect_equal(seeds[[1]]$original_pmid, "12345")
  expect_equal(seeds[[1]]$resolved_status, "pending")

  expect_equal(seeds[[2]]$record_id, "R3")
  expect_equal(seeds[[2]]$notice_type, "Expression of concern")
  expect_equal(seeds[[2]]$notice_date, "2026-03-07")
  expect_equal(seeds[[2]]$original_doi, "10.2000/xyz-789")
  expect_equal(seeds[[2]]$original_pmid, "67890")
})

test_that("load_seed_rows drops Unavailable DOI to NA", {
  csv <- withr::local_tempfile(fileext = ".csv")
  df <- data.frame(
    `Record ID` = "R1",
    Title = "x",
    RetractionNature = "Retraction",
    RetractionDate = "1/21/2026 0:00",
    OriginalPaperDate = "1/21/2020 0:00",
    OriginalPaperDOI = "Unavailable",
    OriginalPaperPubMedID = "",
    Author = "", Journal = "", Publisher = "", Subject = "",
    Reason = "", ArticleType = "", Country = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  readr::write_csv(df, csv)
  seeds <- load_seed_rows(csv)
  expect_length(seeds, 1)
  expect_true(is.na(seeds[[1]]$original_doi))
})

test_that("row_to_seed falls back to RecordID and serializes source_row_json", {
  row <- list(
    RecordID = "REC-99",
    Title = "  Spaced title  ",
    RetractionNature = "Retraction",
    RetractionDate = "1/21/2026 0:00",
    OriginalPaperDate = "6/1/2020 0:00",
    OriginalPaperDOI = "10.1000/ABC123",
    OriginalPaperPubMedID = "111",
    Author = "First Last",
    Journal = "J",
    Publisher = "P",
    Subject = "S",
    Reason = "R",
    ArticleType = "A",
    Country = "C"
  )
  seed <- row_to_seed(row)
  expect_equal(seed$record_id, "REC-99")
  expect_equal(seed$notice_date, "2026-01-21")
  expect_false(is.null(seed$source_row_json))
  expect_true(jsonlite::validate(seed$source_row_json))
})

test_that("search_fallback_terms extracts title and author last name", {
  seed <- list(
    title = "  Some Title  ",
    author = "John A. Doe; Jane Roe"
  )
  terms <- search_fallback_terms(seed)
  expect_equal(terms$title, "Some Title")
  expect_equal(terms$author_last_name, "Doe")
})