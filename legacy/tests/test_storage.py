from retraction_pollution.storage import StudyStore
from retraction_pollution.util import clean_doi


def test_upsert_seed_updates_timestamp_without_binder_error(tmp_path):
    db_path = tmp_path / "study.duckdb"
    seed = {
        "record_id": "1",
        "title": "Retracted paper",
        "notice_type": "Retraction",
        "notice_date": "2024-01-01",
        "original_paper_date": "2020-01-01",
        "original_doi": "10.1/example",
        "original_pmid": None,
        "author": "A Smith",
        "journal": "Journal",
        "publisher": "Publisher",
        "subject": "Subject",
        "reason": "Reason",
        "article_type": "Research article",
        "country": "US",
        "openalex_id": None,
        "resolved_by": None,
        "resolved_status": "pending",
        "source_row_json": "{}",
    }

    with StudyStore(db_path) as store:
        assert store.upsert_seeds([seed]) == 1
        seed["title"] = "Updated title"
        assert store.upsert_seeds([seed]) == 1
        row = store.con.execute("SELECT title FROM seeds WHERE record_id='1'").fetchone()

    assert row[0] == "Updated title"


def test_unresolved_seed_null_doi_can_be_cleaned_after_fetchdf(tmp_path):
    db_path = tmp_path / "study.duckdb"
    seed = {
        "record_id": "1",
        "title": "Paper without DOI",
        "notice_type": "Retraction",
        "notice_date": "2024-01-01",
        "original_paper_date": "2020-01-01",
        "original_doi": None,
        "original_pmid": None,
        "author": None,
        "journal": None,
        "publisher": None,
        "subject": None,
        "reason": None,
        "article_type": None,
        "country": None,
        "openalex_id": None,
        "resolved_by": None,
        "resolved_status": "pending",
        "source_row_json": "{}",
    }

    with StudyStore(db_path) as store:
        store.upsert_seed(seed)
        fetched = store.unresolved_seeds()[0]

    assert clean_doi(fetched["original_doi"]) is None
