library(testthat)

# Source util.R directly so tests run without a built/installed package.
util_path <- file.path(testthat::test_path("..", "..", "R", "util.R"))
if (file.exists(util_path)) source(util_path)

test_that("is_missing detects NA, NaN, NULL, and length-0", {
  expect_true(is_missing(NA))
  expect_true(is_missing(NaN))
  expect_true(is_missing(NULL))
  expect_true(is_missing(character(0)))
  expect_false(is_missing("x"))
  expect_false(is_missing(0))
})

test_that("text_or_none trims and returns NA for missing", {
  expect_equal(text_or_none("  hello  "), "hello")
  expect_equal(text_or_none(NA_character_), NA_character_)
  expect_equal(text_or_none(""), NA_character_)
  expect_equal(text_or_none(NaN), NA_character_)
  expect_equal(text_or_none("   "), NA_character_)
})

test_that("clean_doi strips URL prefixes and lowercases", {
  expect_equal(clean_doi("https://doi.org/10.1234/ABC"), "10.1234/abc")
  expect_equal(clean_doi("http://dx.doi.org/10.1234/abc"), "10.1234/abc")
  expect_equal(clean_doi("doi: 10.1234/ABC"), "10.1234/abc")
  expect_equal(clean_doi("DOI: 10.1234/XYZ"), "10.1234/xyz")
  expect_equal(clean_doi("10.1234/ABC"), "10.1234/abc")
})

test_that("clean_doi rejects unavailable sentinels", {
  expect_equal(clean_doi(""), NA_character_)
  expect_equal(clean_doi("unavailable"), NA_character_)
  expect_equal(clean_doi("Unavailable"), NA_character_)
  expect_equal(clean_doi("n/a"), NA_character_)
  expect_equal(clean_doi("na"), NA_character_)
  expect_equal(clean_doi("none"), NA_character_)
  expect_equal(clean_doi("null"), NA_character_)
  expect_equal(clean_doi("0"), NA_character_)
  expect_equal(clean_doi(NA_character_), NA_character_)
  expect_equal(clean_doi(NaN), NA_character_)
})

test_that("clean_doi extracts DOI embedded in surrounding text", {
  expect_equal(clean_doi("see https://doi.org/10.1234/abc for details"),
               "10.1234/abc")
  expect_equal(clean_doi("prefix 10.5555/xyz.suffix"), "10.5555/xyz.suffix")
})

test_that("doi_url builds a https URL", {
  expect_equal(doi_url("10.1234/abc"), "https://doi.org/10.1234/abc")
  expect_equal(doi_url("https://doi.org/10.1234/ABC"),
               "https://doi.org/10.1234/abc")
  expect_equal(doi_url("unavailable"), NA_character_)
})

test_that("clean_pmid extracts digits and rejects sentinels", {
  expect_equal(clean_pmid("PMID: 123456"), "123456")
  expect_equal(clean_pmid("123456"), "123456")
  expect_equal(clean_pmid("0"), NA_character_)
  expect_equal(clean_pmid("0.0"), NA_character_)
  expect_equal(clean_pmid("unavailable"), NA_character_)
  expect_equal(clean_pmid("n/a"), NA_character_)
  expect_equal(clean_pmid("na"), NA_character_)
  expect_equal(clean_pmid(NA_character_), NA_character_)
})

test_that("clean_pmid handles numeric input", {
  expect_equal(clean_pmid(123456.0), "123456")
  expect_equal(clean_pmid(0), NA_character_)
})

test_that("parse_date handles the CRITICAL Retraction Watch M/D/YYYY 0:00 format", {
  # THE regression test — this is what the Python original failed on.
  expect_equal(parse_date("1/21/2026 0:00"), "2026-01-21")
  expect_equal(parse_date("10/15/2022 0:00"), "2022-10-15")
})

test_that("parse_date handles other supported formats", {
  expect_equal(parse_date("2026-01-21"), "2026-01-21")
  expect_equal(parse_date("2026-01"), "2026-01-01")
  expect_equal(parse_date("2026"), "2026-01-01")
  expect_equal(parse_date("1/21/2026"), "2026-01-21")
  expect_equal(parse_date("2026/01/21"), "2026-01-21")
  expect_equal(parse_date("10/15/2022"), "2022-10-15")
})

test_that("parse_date returns NA for invalid/sentinel values", {
  expect_equal(parse_date("0000-00-00"), NA_character_)
  expect_equal(parse_date("0"), NA_character_)
  expect_equal(parse_date(""), NA_character_)
  expect_equal(parse_date(NA_character_), NA_character_)
})

test_that("compact_openalex_id strips the prefix", {
  expect_equal(compact_openalex_id("https://openalex.org/W123"), "W123")
  expect_equal(compact_openalex_id("W123"), "W123")
  expect_equal(compact_openalex_id(NA_character_), NA_character_)
  expect_equal(compact_openalex_id(""), NA_character_)
})

test_that("full_openalex_id adds the prefix back", {
  expect_equal(full_openalex_id("W123"), "https://openalex.org/W123")
  expect_equal(full_openalex_id("https://openalex.org/W123"),
               "https://openalex.org/W123")
  expect_equal(full_openalex_id(NA_character_), NA_character_)
})

test_that("first_author_last_name takes the last token of the first author", {
  expect_equal(first_author_last_name("Smith; Jones; Brown"), "Smith")
  expect_equal(first_author_last_name("Jane Q. Smith; Jones"), "Smith")
  expect_equal(first_author_last_name("Smith"), "Smith")
  expect_equal(first_author_last_name(""), NA_character_)
  expect_equal(first_author_last_name(NA_character_), NA_character_)
  expect_equal(first_author_last_name("  ;  "), NA_character_)
})

test_that("chunked splits into correctly-sized groups", {
  res <- chunked(1:10, 3)
  expect_type(res, "list")
  expect_length(res, 4)
  expect_equal(lengths(res), c(3, 3, 3, 1))
  expect_equal(res[[1]], 1:3)
  expect_equal(res[[4]], 10)
})

test_that("chunked handles edge cases", {
  expect_length(chunked(integer(0), 3), 0)
  expect_length(chunked(1:5, 10), 1)
  expect_equal(chunked(1:5, 10)[[1]], 1:5)
})

test_that("stable_hash is order-independent and a string", {
  h1 <- stable_hash(c("a", "b"))
  h2 <- stable_hash(c("b", "a"))
  expect_type(h1, "character")
  expect_equal(nchar(h1), 8)
  expect_equal(h1, h2)
})

test_that("stable_hash is deterministic", {
  expect_equal(stable_hash(c("x", "y", "z")),
               stable_hash(c("z", "y", "x")))
})

test_that("json_dumps produces a character string", {
  s <- json_dumps(list(a = 1, b = "x"))
  expect_type(s, "character")
  expect_true(jsonlite::validate(s))
})

test_that("ensure_dir creates a directory", {
  tmp <- file.path(tempdir(), "rp_util_test_dir")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  expect_equal(ensure_dir(tmp), tmp)
  expect_true(dir.exists(tmp))
  # idempotent
  expect_equal(ensure_dir(tmp), tmp)
  unlink(tmp, recursive = TRUE)
})