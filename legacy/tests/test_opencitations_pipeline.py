from retraction_pollution.opencitations import OpenCitation
from retraction_pollution.opencitations_pipeline import (
    OpenCitationsOnlyCrawler,
    prepare_opencitations_seeds,
)
from retraction_pollution.storage import StudyStore


class FakeOpenCitationsClient:
    def __init__(self):
        self.calls = []

    def citations_by_doi(self, doi):
        self.calls.append(doi)
        if doi == "10.seed/a":
            return [
                OpenCitation(
                    citing_doi="10.citer/b",
                    citing_pmid=None,
                    cited_doi="10.seed/a",
                    creation_date="2024-01-01",
                    raw={"citing": "doi:10.citer/b", "cited": "doi:10.seed/a"},
                )
            ]
        if doi == "10.citer/b":
            return [
                OpenCitation(
                    citing_doi="10.depth2/c",
                    citing_pmid=None,
                    cited_doi="10.citer/b",
                    creation_date="2025-01-01",
                    raw={"citing": "doi:10.depth2/c", "cited": "doi:10.citer/b"},
                )
            ]
        return []


def test_opencitations_only_pipeline_builds_doi_graph(tmp_path):
    seeds = [
        {
            "record_id": "1",
            "title": "Seed paper",
            "notice_type": "Retraction",
            "notice_date": "2023-01-01",
            "original_paper_date": "2020-01-01",
            "original_doi": "10.seed/a",
            "original_pmid": None,
            "author": "A Smith",
            "journal": "Journal",
            "publisher": "Publisher",
            "subject": "Biology",
            "reason": "Reason",
            "article_type": "Research",
            "country": "US",
            "openalex_id": None,
            "resolved_by": None,
            "resolved_status": "pending",
            "source_row_json": "{}",
        }
    ]

    with StudyStore(tmp_path / "opencitations.duckdb") as store:
        seed_stats = prepare_opencitations_seeds(store, seeds)
        summary = OpenCitationsOnlyCrawler(store, FakeOpenCitationsClient()).crawl(max_depth=2)
        edges = store.con.execute(
            "SELECT source_id, target_id, depth, source_api FROM citation_edges ORDER BY depth"
        ).fetchall()
        nodes = store.con.execute(
            "SELECT openalex_id, depth FROM frontier_nodes ORDER BY depth, openalex_id"
        ).fetchall()

    assert seed_stats["doi_seeds"] == 1
    assert summary["seed_nodes"] == 1
    assert edges == [
        ("doi:10.citer/b", "doi:10.seed/a", 1, "opencitations"),
        ("doi:10.depth2/c", "doi:10.citer/b", 2, "opencitations"),
    ]
    assert nodes == [
        ("doi:10.seed/a", 0),
        ("doi:10.citer/b", 1),
        ("doi:10.depth2/c", 2),
    ]
