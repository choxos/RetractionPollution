from retraction_pollution.openalex import (
    OpenAlexClient,
    OpenAlexError,
    _openalex_doi_url,
    edges_from_work,
    normalize_work,
)


class FailingBatchOpenAlexClient(OpenAlexClient):
    def __init__(self):
        super().__init__(api_key="test", request_delay=0)
        self.filters = []

    def _request_json(self, path, params=None):
        self.filters.append(params["filter"])
        if "|" in params["filter"]:
            raise OpenAlexError("bad batch", status_code=400)
        if "10.1234/good" in params["filter"]:
            return {"results": [{"id": "https://openalex.org/W1", "doi": "https://doi.org/10.1234/good"}]}
        raise OpenAlexError("bad singleton", status_code=400)


def test_normalize_work_extracts_core_metadata():
    work = {
        "id": "https://openalex.org/W1",
        "doi": "https://doi.org/10.1/ABC",
        "display_name": "A work",
        "publication_date": "2024-01-01",
        "publication_year": 2024,
        "type": "article",
        "is_retracted": False,
        "cited_by_count": 5,
        "referenced_works": ["https://openalex.org/W0"],
        "primary_location": {"source": {"id": "https://openalex.org/S1", "display_name": "J"}},
        "primary_topic": {
            "id": "https://openalex.org/T1",
            "display_name": "Topic",
            "domain": {"display_name": "Health sciences"},
        },
    }

    row = normalize_work(work)

    assert row["openalex_id"] == "W1"
    assert row["doi"] == "10.1/abc"
    assert row["source_id"] == "S1"
    assert row["topic_domain"] == "Health sciences"


def test_edges_from_work_intersects_references_with_parent_set():
    work = {
        "id": "https://openalex.org/W2",
        "referenced_works": ["https://openalex.org/W0", "https://openalex.org/W9"],
    }

    assert edges_from_work(work, {"W0", "W1"}) == [("W2", "W0")]


def test_openalex_doi_filter_skips_values_that_openalex_rejects():
    assert _openalex_doi_url("10.1234/valid") == "https://doi.org/10.1234/valid"
    assert _openalex_doi_url("10.1234/with,comma") is None
    assert _openalex_doi_url("10.1234/with&ampersand") is None


def test_get_works_by_dois_bisects_bad_batches_and_skips_bad_singletons():
    client = FailingBatchOpenAlexClient()

    works = client.get_works_by_dois(["10.1234/good", "10.1234/bad"])

    assert [work["id"] for work in works] == ["https://openalex.org/W1"]
    assert len(client.filters) == 3
