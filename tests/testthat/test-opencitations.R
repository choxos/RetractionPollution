test_that("extract_pid handles multi-PID strings, missing prefixes, and NA", {
  s <- "doi:10.1/a pmid:123456 pmcid:PMC789"
  expect_equal(extract_pid(s, "doi"), "10.1/a")
  expect_equal(extract_pid(s, "pmid"), "123456")
  # No pmcid extractor handler — but the prefix is present and should be
  # returned as-is (we only extract the captured group, no pmcid cleaner).
  expect_equal(extract_pid(s, "pmcid"), "PMC789")
  # Prefix not present at all -> NA.
  expect_equal(extract_pid(s, "orcid"), NA_character_)
  # NA input -> NA.
  expect_equal(extract_pid(NA_character_, "doi"), NA_character_)
  expect_equal(extract_pid(NULL, "doi"), NA_character_)
  # Empty string -> NA.
  expect_equal(extract_pid("", "doi"), NA_character_)
  # Prefix anchored at start works (no leading space).
  expect_equal(extract_pid("doi:10.1/a", "doi"), "10.1/a")
})

test_that("parse_open_citation normalizes fields correctly", {
  item <- list(
    citing = "doi:10.1234/Test",
    cited = "doi:10.5678/ABC",
    creation = "2024-02"
  )
  oc <- parse_open_citation(item)
  expect_s3_class(oc, "OpenCitation")
  expect_equal(oc$citing_doi, "10.1234/test")
  expect_equal(oc$cited_doi, "10.5678/abc")
  expect_equal(oc$creation_date, "2024-02-01")
  # No pmid in `citing` -> NA.
  expect_true(is.na(oc$citing_pmid))
  # raw preserves the input list.
  expect_equal(oc$raw, item)
})

test_that("parse_open_citation with missing fields returns NAs", {
  oc <- parse_open_citation(list())
  expect_s3_class(oc, "OpenCitation")
  expect_true(is.na(oc$citing_doi))
  expect_true(is.na(oc$cited_doi))
  expect_true(is.na(oc$creation_date))
  expect_true(is.na(oc$citing_pmid))
  expect_equal(oc$raw, list())
})

test_that("doi_node_id builds doi: prefixed IDs", {
  expect_equal(doi_node_id("10.1234/test"), "doi:10.1234/test")
  expect_equal(doi_node_id("10.1234/TEST"), "doi:10.1234/test")
  expect_equal(doi_node_id(NA_character_), NA_character_)
  expect_equal(doi_node_id(NULL), NA_character_)
  expect_equal(doi_node_id(""), NA_character_)
})

test_that("doi_from_node_id extracts bare DOI", {
  expect_equal(doi_from_node_id("doi:10.1234/test"), "10.1234/test")
  expect_equal(doi_from_node_id("doi:10.1234/TEST"), "10.1234/test")
  expect_equal(doi_from_node_id("pmid:123"), NA_character_)
  expect_equal(doi_from_node_id(NA_character_), NA_character_)
  expect_equal(doi_from_node_id(NULL), NA_character_)
  expect_equal(doi_from_node_id("10.1234/test"), NA_character_)
})

test_that("pmid_node_id builds pmid: prefixed IDs", {
  expect_equal(pmid_node_id("123456"), "pmid:123456")
  expect_equal(pmid_node_id(NA_character_), NA_character_)
  expect_equal(pmid_node_id(NULL), NA_character_)
  expect_equal(pmid_node_id(""), NA_character_)
  expect_equal(pmid_node_id("0"), NA_character_)
})

test_that("OpenCitationsClient constructs with and without a token", {
  skip_if_offline()
  client <- OpenCitationsClient()
  expect_s3_class(client, "OpenCitationsClient")
  expect_null(client$token)
  expect_equal(client$index_base_url, OPENCITATIONS_INDEX_API)
  expect_equal(client$meta_base_url, OPENCITATIONS_META_API)
  expect_equal(client$retries, 5L)
  expect_equal(client$request_delay, 0.2)

  tok <- OpenCitationsClient(token = "my-token")
  expect_equal(tok$token, "my-token")
})

test_that("OpenCitationsClient trims trailing slashes from base URLs", {
  client <- OpenCitationsClient(
    index_base_url = "https://example.com/index/v2/",
    meta_base_url = "https://example.com/meta/v1///"
  )
  expect_equal(client$index_base_url, "https://example.com/index/v2")
  expect_equal(client$meta_base_url, "https://example.com/meta/v1")
})

test_that("OpenCitationsClient$citations_by_doi returns empty for NA DOI", {
  client <- OpenCitationsClient()
  expect_equal(client$citations_by_doi(NA_character_), list())
  expect_equal(client$citations_by_doi(""), list())
  expect_equal(client$citations_by_doi(NULL), list())
})

test_that("OpenCitationsClient$metadata_by_doi returns NULL for NA DOI", {
  client <- OpenCitationsClient()
  expect_null(client$metadata_by_doi(NA_character_))
  expect_null(client$metadata_by_doi(""))
  expect_null(client$metadata_by_doi(NULL))
})

test_that("OpenCitation is a simple data carrier", {
  oc <- OpenCitation(
    citing_doi = "10.1/a",
    citing_pmid = "123",
    cited_doi = "10.2/b",
    creation_date = "2024-01-01",
    raw = list(x = 1)
  )
  expect_s3_class(oc, "OpenCitation")
  expect_equal(oc$citing_doi, "10.1/a")
  expect_equal(oc$citing_pmid, "123")
  expect_equal(oc$cited_doi, "10.2/b")
  expect_equal(oc$creation_date, "2024-01-01")
  expect_equal(oc$raw, list(x = 1))
})

test_that("OpenCitation defaults to NAs and empty raw", {
  oc <- OpenCitation()
  expect_s3_class(oc, "OpenCitation")
  expect_true(is.na(oc$citing_doi))
  expect_true(is.na(oc$citing_pmid))
  expect_true(is.na(oc$cited_doi))
  expect_true(is.na(oc$creation_date))
  expect_equal(oc$raw, list())
})

test_that("OpenCitationsError can be constructed and raised", {
  err <- open_citations_error("boom")
  expect_s3_class(err, "OpenCitationsError")
  expect_s3_class(err, "error")
  expect_equal(err$message, "boom")
  expect_error(stop_open_citations("boom"), class = "OpenCitationsError")
})