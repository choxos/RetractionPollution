from retraction_pollution.crawler import resolve_pending_seeds
from retraction_pollution.storage import StudyStore


class FakeOpenAlexClient:
    def get_works_by_dois(self, dois):
        raise AssertionError(f"DOI lookup should not be called for missing DOI values: {dois}")

    def get_work_by_pmid(self, pmid):
        raise AssertionError(f"PMID lookup should not be called for missing PMID values: {pmid}")

    def search_work(self, title, author_last_name=None):
        raise AssertionError(f"Title search should not be called for missing title values: {title}")


def test_resolve_pending_seeds_handles_missing_fetchdf_values(tmp_path):
    seed = {
        "record_id": "1",
        "title": None,
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

    with StudyStore(tmp_path / "study.duckdb") as store:
        store.upsert_seed(seed)
        stats = resolve_pending_seeds(store, FakeOpenAlexClient())
        status = store.con.execute("SELECT resolved_status FROM seeds").fetchone()[0]

    assert stats["pending_title_fallback"] == 1
    assert status == "pending_title_fallback"


def test_resolve_pending_seeds_marks_no_identifier_records_before_network(tmp_path):
    seed = {
        "record_id": "1",
        "title": "Title-only paper",
        "notice_type": "Retraction",
        "notice_date": "2024-01-01",
        "original_paper_date": "2020-01-01",
        "original_doi": None,
        "original_pmid": None,
        "author": "A Smith",
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

    with StudyStore(tmp_path / "study.duckdb") as store:
        store.upsert_seed(seed)
        stats = resolve_pending_seeds(store, FakeOpenAlexClient())
        row = store.con.execute(
            "SELECT resolved_by, resolved_status FROM seeds WHERE record_id='1'"
        ).fetchone()

    assert stats["pending_title_fallback"] == 1
    assert row == ("exact", "pending_title_fallback")


def test_resolve_pending_seeds_marks_not_found_when_title_fallback_enabled(tmp_path):
    seed = {
        "record_id": "1",
        "title": None,
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
        "resolved_status": "pending_title_fallback",
        "source_row_json": "{}",
    }

    with StudyStore(tmp_path / "study.duckdb") as store:
        store.upsert_seed(seed)
        stats = resolve_pending_seeds(store, FakeOpenAlexClient(), title_fallback=True)
        status = store.con.execute("SELECT resolved_status FROM seeds").fetchone()[0]

    assert stats["not_found"] == 1
    assert status == "not_found"
