from retraction_pollution.util import (
    clean_doi,
    clean_pmid,
    compact_openalex_id,
    first_author_last_name,
    parse_date,
)


def test_clean_doi_normalizes_common_forms():
    assert clean_doi("https://doi.org/10.123/ABC.") == "10.123/abc"
    assert clean_doi("doi:10.555/Test") == "10.555/test"
    assert clean_doi("10.1234/abc; 10.5678/def") == "10.1234/abc"
    assert clean_doi("Unavailable") is None
    assert clean_doi(float("nan")) is None


def test_clean_pmid_extracts_numeric_identifier():
    assert clean_pmid("PMID: 123456") == "123456"
    assert clean_pmid(123456.0) == "123456"
    assert clean_pmid("0") is None
    assert clean_pmid(float("nan")) is None


def test_parse_date_handles_partial_dates():
    assert parse_date("2020") == "2020-01-01"
    assert parse_date("2020-07") == "2020-07-01"
    assert parse_date("2020-07-15") == "2020-07-15"
    assert parse_date("") is None
    assert parse_date(float("nan")) is None


def test_missing_values_do_not_break_text_helpers():
    assert compact_openalex_id(float("nan")) is None
    assert first_author_last_name(float("nan")) is None
