from __future__ import annotations

from typing import Any

from .opencitations import (
    OpenCitation,
    OpenCitationsClient,
    OpenCitationsError,
    doi_from_node_id,
    doi_node_id,
    pmid_node_id,
)
from .storage import StudyStore
from .util import clean_doi, json_dumps, parse_date


def prepare_opencitations_seeds(
    store: StudyStore,
    seeds: list[dict[str, Any]],
) -> dict[str, int]:
    stats = {"loaded": 0, "doi_seeds": 0, "no_doi": 0}
    for seed in seeds:
        stats["loaded"] += 1
        store.upsert_seed(seed)
        doi = clean_doi(seed.get("original_doi"))
        node_id = doi_node_id(doi)
        if not node_id:
            store.update_seed_resolution(
                seed["record_id"], None, "opencitations_doi", "no_doi"
            )
            stats["no_doi"] += 1
            continue
        store.upsert_work(
            {
                "openalex_id": node_id,
                "doi": doi,
                "title": seed.get("title"),
                "publication_date": seed.get("original_paper_date"),
                "publication_year": _year(seed.get("original_paper_date")),
                "work_type": seed.get("article_type"),
                "is_retracted": True,
                "cited_by_count": None,
                "source_id": None,
                "source_name": seed.get("journal"),
                "topic_id": None,
                "topic_name": seed.get("subject"),
                "topic_domain": None,
                "referenced_works_json": "[]",
                "raw_json": seed.get("source_row_json") or json_dumps(seed),
            }
        )
        store.update_seed_resolution(seed["record_id"], node_id, "opencitations_doi", "resolved")
        stats["doi_seeds"] += 1
    store.set_metadata("pipeline_mode", "opencitations")
    store.set_metadata("opencitations_seed_stats", stats)
    return stats


class OpenCitationsOnlyCrawler:
    def __init__(self, store: StudyStore, client: OpenCitationsClient):
        self.store = store
        self.client = client

    def crawl(
        self,
        *,
        max_depth: int = 3,
        complete_depth: int = 2,
        depth3_node_cap: int = 250_000,
        parent_limit: int | None = None,
    ) -> dict[str, Any]:
        seed_ids = self.store.resolved_seed_ids()
        for seed_id in seed_ids:
            self.store.add_frontier_node(seed_id, 0)
        summary: dict[str, Any] = {
            "mode": "opencitations",
            "seed_nodes": len(seed_ids),
            "max_depth": max_depth,
            "complete_depth": complete_depth,
            "levels": {},
        }
        for current_depth in range(max_depth):
            target_depth = current_depth + 1
            stats = self._crawl_level(
                current_depth=current_depth,
                target_depth=target_depth,
                capped=target_depth > complete_depth,
                depth3_node_cap=depth3_node_cap,
                parent_limit=parent_limit,
            )
            summary["levels"][str(target_depth)] = stats
            if stats["truncated"]:
                break
        self.store.set_metadata("last_crawl_summary", summary)
        return summary

    def _crawl_level(
        self,
        *,
        current_depth: int,
        target_depth: int,
        capped: bool,
        depth3_node_cap: int,
        parent_limit: int | None,
    ) -> dict[str, Any]:
        parents = self.store.pending_frontier(current_depth, limit=parent_limit)
        stats: dict[str, Any] = {
            "parent_count": len(parents),
            "parents_queried": 0,
            "parents_failed": 0,
            "parents_without_doi": 0,
            "citations": 0,
            "new_depth_count_start": self.store.count_frontier_depth(target_depth),
            "new_depth_count_end": None,
            "truncated": False,
        }
        for parent_id in parents:
            if capped and self.store.count_frontier_depth(target_depth) >= depth3_node_cap:
                stats["truncated"] = True
                self.store.set_metadata(f"depth{target_depth}_truncated", True)
                break
            doi = doi_from_node_id(parent_id)
            if not doi:
                stats["parents_without_doi"] += 1
                self.store.mark_processed([parent_id])
                continue
            try:
                citations = self.client.citations_by_doi(doi)
            except OpenCitationsError as exc:
                stats["parents_failed"] += 1
                self.store.set_metadata(f"opencitations_failed_parent:{parent_id}", str(exc))
                continue
            stats["parents_queried"] += 1
            stats["citations"] += len(citations)
            self._store_citations(parent_id, citations, target_depth)
            self.store.mark_processed([parent_id])
        stats["new_depth_count_end"] = self.store.count_frontier_depth(target_depth)
        return stats

    def _store_citations(
        self,
        parent_id: str,
        citations: list[OpenCitation],
        target_depth: int,
    ) -> None:
        parent_doi = doi_from_node_id(parent_id)
        for citation in citations:
            source_id = doi_node_id(citation.citing_doi) or pmid_node_id(citation.citing_pmid)
            if not source_id:
                continue
            self.store.upsert_work(
                {
                    "openalex_id": source_id,
                    "doi": clean_doi(citation.citing_doi),
                    "title": None,
                    "publication_date": citation.creation_date,
                    "publication_year": _year(citation.creation_date),
                    "work_type": None,
                    "is_retracted": False,
                    "cited_by_count": None,
                    "source_id": None,
                    "source_name": None,
                    "topic_id": None,
                    "topic_name": None,
                    "topic_domain": None,
                    "referenced_works_json": json_dumps([parent_id]),
                    "raw_json": json_dumps(citation.raw),
                }
            )
            self.store.add_frontier_node(source_id, target_depth)
            self.store.add_edge(
                source_id,
                doi_node_id(citation.cited_doi) or parent_id,
                target_depth,
                source_api="opencitations",
                citation_date=citation.creation_date,
            )
            if parent_doi and citation.cited_doi and clean_doi(citation.cited_doi) != parent_doi:
                self.store.add_edge(
                    source_id,
                    parent_id,
                    target_depth,
                    source_api="opencitations",
                    citation_date=citation.creation_date,
                )


def _year(value: Any) -> int | None:
    parsed = parse_date(value)
    return int(parsed[:4]) if parsed else None
