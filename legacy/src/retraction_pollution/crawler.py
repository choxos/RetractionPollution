from __future__ import annotations

from collections.abc import Iterable
from typing import Any

from .openalex import OpenAlexClient, edges_from_work, normalize_work
from .opencitations import OpenCitation, OpenCitationsClient
from .rw import search_fallback_terms
from .storage import StudyStore
from .util import chunked, clean_doi, clean_pmid, compact_openalex_id, stable_hash


def resolve_pending_seeds(
    store: StudyStore,
    client: OpenAlexClient,
    *,
    limit: int | None = None,
    batch_size: int = 100,
    title_fallback: bool = False,
) -> dict[str, int]:
    seeds = store.unresolved_seeds(
        limit=limit,
        include_pending_title_fallback=title_fallback,
    )
    stats = {"resolved": 0, "not_found": 0, "checked": 0, "pending_title_fallback": 0}

    if not title_fallback:
        seeds_with_no_exact_id = [
            seed
            for seed in seeds
            if not clean_doi(seed.get("original_doi"))
            and not clean_pmid(seed.get("original_pmid"))
        ]
        for seed in seeds_with_no_exact_id:
            store.update_seed_resolution(
                seed["record_id"], None, "exact", "pending_title_fallback"
            )
            stats["pending_title_fallback"] += 1
        seeds = [
            seed
            for seed in seeds
            if clean_doi(seed.get("original_doi")) or clean_pmid(seed.get("original_pmid"))
        ]

    doi_seeds = [seed for seed in seeds if clean_doi(seed.get("original_doi"))]
    for batch in chunked(doi_seeds, min(batch_size, 100)):
        works = client.get_works_by_dois([seed["original_doi"] for seed in batch])
        by_doi = {clean_doi(work.get("doi")): work for work in works if clean_doi(work.get("doi"))}
        for seed in batch:
            stats["checked"] += 1
            work = by_doi.get(clean_doi(seed.get("original_doi")))
            if work and work.get("id"):
                row = normalize_work(work)
                store.upsert_work(row)
                store.update_seed_resolution(
                    seed["record_id"], row["openalex_id"], "doi", "resolved"
                )
                stats["resolved"] += 1

    remaining = store.unresolved_seeds(
        limit=limit,
        include_pending_title_fallback=title_fallback,
    )
    for seed in remaining:
        pmid = clean_pmid(seed.get("original_pmid"))
        if pmid:
            stats["checked"] += 1
            work = client.get_work_by_pmid(pmid)
            if work and work.get("id"):
                row = normalize_work(work)
                store.upsert_work(row)
                store.update_seed_resolution(
                    seed["record_id"], row["openalex_id"], "pmid", "resolved"
                )
                stats["resolved"] += 1
                continue
        if title_fallback:
            title, author_last_name = search_fallback_terms(seed)
            if title:
                stats["checked"] += 1
                work = client.search_work(title, author_last_name)
                if work and work.get("id"):
                    row = normalize_work(work)
                    store.upsert_work(row)
                    store.update_seed_resolution(
                        seed["record_id"], row["openalex_id"], "title_author", "resolved"
                    )
                    stats["resolved"] += 1
                    continue
        if title_fallback:
            store.update_seed_resolution(seed["record_id"], None, "all", "not_found")
            stats["not_found"] += 1
        else:
            store.update_seed_resolution(
                seed["record_id"], None, "exact", "pending_title_fallback"
            )
            stats["pending_title_fallback"] += 1

    return stats


class CitationCrawler:
    def __init__(
        self,
        store: StudyStore,
        openalex: OpenAlexClient,
        opencitations: OpenCitationsClient | None = None,
    ):
        self.store = store
        self.openalex = openalex
        self.opencitations = opencitations

    def supplement_level_from_opencitations(
        self,
        *,
        current_depth: int,
        target_depth: int,
        batch_size: int = 100,
        parent_limit: int | None = None,
        depth3_node_cap: int = 250_000,
    ) -> dict[str, int]:
        if self.opencitations is None:
            return {"parents": 0, "citations": 0, "resolved_depth_nodes": 0, "truncated": False}
        parents = self.store.frontier_with_doi(current_depth)
        if parent_limit is not None:
            parents = parents[:parent_limit]
        stats = {"parents": 0, "citations": 0, "resolved_depth_nodes": 0, "truncated": False}
        for parent in parents:
            if target_depth == 3 and self.store.count_frontier_depth(3) >= depth3_node_cap:
                stats["truncated"] = True
                self.store.set_metadata("depth3_truncated", True)
                break
            stats["parents"] += 1
            citations = self.opencitations.citations_by_doi(parent["doi"])
            stats["citations"] += len(citations)
            self._store_opencitations_for_parent(
                parent["openalex_id"],
                citations,
                target_depth,
                batch_size,
                depth3_node_cap=depth3_node_cap,
            )
            stats["resolved_depth_nodes"] = self.store.count_frontier_depth(target_depth)
        self.store.set_metadata(f"opencitations_depth{target_depth}_supplement", stats)
        return stats

    def _store_opencitations_for_parent(
        self,
        parent_openalex_id: str,
        citations: list[OpenCitation],
        target_depth: int,
        batch_size: int,
        *,
        depth3_node_cap: int,
    ) -> None:
        by_doi = {
            citation.citing_doi: citation
            for citation in citations
            if citation.citing_doi is not None
        }
        for doi_batch in chunked(list(by_doi.keys()), min(batch_size, 100)):
            if target_depth == 3 and self.store.count_frontier_depth(3) >= depth3_node_cap:
                return
            works = self.openalex.get_works_by_dois(doi_batch)
            for work in works:
                row = normalize_work(work)
                source_id = row.get("openalex_id")
                citing_doi = clean_doi(row.get("doi"))
                if not source_id or not citing_doi:
                    continue
                citation = by_doi.get(citing_doi)
                self.store.upsert_work(row)
                self.store.add_frontier_node(source_id, target_depth)
                self.store.add_edge(
                    source_id,
                    compact_openalex_id(parent_openalex_id),
                    target_depth,
                    source_api="opencitations",
                    citation_date=citation.creation_date if citation else None,
                )
        pmid_citations = [
            citation
            for citation in citations
            if citation.citing_doi is None and citation.citing_pmid is not None
        ]
        for citation in pmid_citations:
            if target_depth == 3 and self.store.count_frontier_depth(3) >= depth3_node_cap:
                return
            work = self.openalex.get_work_by_pmid(citation.citing_pmid or "")
            if not work:
                continue
            row = normalize_work(work)
            source_id = row.get("openalex_id")
            if not source_id:
                continue
            self.store.upsert_work(row)
            self.store.add_frontier_node(source_id, target_depth)
            self.store.add_edge(
                source_id,
                compact_openalex_id(parent_openalex_id),
                target_depth,
                source_api="opencitations",
                citation_date=citation.creation_date,
            )

    def crawl(
        self,
        *,
        max_depth: int = 3,
        complete_depth: int = 2,
        batch_size: int = 100,
        per_page: int = 100,
        depth3_node_cap: int = 250_000,
        depth3_page_cap: int = 2_500,
    ) -> dict[str, Any]:
        seed_ids = self.store.resolved_seed_ids()
        for seed_id in seed_ids:
            self.store.add_frontier_node(seed_id, 0)

        summary: dict[str, Any] = {
            "seed_nodes": len(seed_ids),
            "max_depth": max_depth,
            "complete_depth": complete_depth,
            "levels": {},
        }
        for current_depth in range(max_depth):
            target_depth = current_depth + 1
            capped = target_depth > complete_depth
            oc_stats = self.supplement_level_from_opencitations(
                current_depth=current_depth,
                target_depth=target_depth,
                batch_size=batch_size,
                depth3_node_cap=depth3_node_cap,
            )
            if oc_stats.get("truncated"):
                summary["levels"][str(target_depth)] = {"opencitations": oc_stats}
                break
            level_stats = self._crawl_level(
                current_depth,
                target_depth,
                capped=capped,
                batch_size=batch_size,
                per_page=per_page,
                depth3_node_cap=depth3_node_cap,
                depth3_page_cap=depth3_page_cap,
            )
            level_stats["opencitations"] = oc_stats
            summary["levels"][str(target_depth)] = level_stats
            if level_stats.get("truncated"):
                break
        self.store.set_metadata("last_crawl_summary", summary)
        return summary

    def _crawl_level(
        self,
        current_depth: int,
        target_depth: int,
        *,
        capped: bool,
        batch_size: int,
        per_page: int,
        depth3_node_cap: int,
        depth3_page_cap: int,
    ) -> dict[str, Any]:
        parents = self.store.pending_frontier(current_depth)
        stats: dict[str, Any] = {
            "parent_count": len(parents),
            "pages": 0,
            "results": 0,
            "new_depth_count_start": self.store.count_frontier_depth(target_depth),
            "truncated": False,
        }
        for parent_batch in chunked(parents, min(batch_size, 100)):
            if capped and self._depth_cap_reached(target_depth, depth3_node_cap, depth3_page_cap):
                stats["truncated"] = True
                self.store.set_metadata(f"depth{target_depth}_truncated", True)
                break
            job_id = f"depth{current_depth}-{stable_hash(parent_batch)}"
            job = self.store.get_or_create_job(job_id, current_depth, parent_batch)
            if bool(job["done"]):
                continue
            parent_set = {compact_openalex_id(parent_id) for parent_id in parent_batch}
            cursor = job["cursor"] or "*"
            while True:
                if capped and self._depth_cap_reached(
                    target_depth, depth3_node_cap, depth3_page_cap
                ):
                    stats["truncated"] = True
                    self.store.set_metadata(f"depth{target_depth}_truncated", True)
                    return stats
                page = self.openalex.list_citers(parent_batch, cursor=cursor, per_page=per_page)
                self._store_page(page.results, parent_set, target_depth)
                stats["pages"] += 1
                stats["results"] += len(page.results)
                if capped:
                    self._increment_page_count(target_depth)
                if page.next_cursor and page.results:
                    cursor = page.next_cursor
                    self.store.update_job(
                        job_id,
                        cursor=cursor,
                        pages_delta=1,
                        results_delta=len(page.results),
                    )
                    continue
                self.store.update_job(
                    job_id,
                    cursor=page.next_cursor or cursor,
                    done=True,
                    pages_delta=1,
                    results_delta=len(page.results),
                )
                self.store.mark_processed(parent_batch)
                break
        stats["new_depth_count_end"] = self.store.count_frontier_depth(target_depth)
        return stats

    def _store_page(
        self, works: Iterable[dict[str, Any]], parent_set: set[str | None], target_depth: int
    ) -> None:
        parent_ids = {parent_id for parent_id in parent_set if parent_id}
        for work in works:
            row = normalize_work(work)
            source_id = row.get("openalex_id")
            if not source_id:
                continue
            self.store.upsert_work(row)
            self.store.add_frontier_node(source_id, target_depth)
            for edge_source, edge_target in edges_from_work(work, parent_ids):
                self.store.add_edge(edge_source, edge_target, target_depth, source_api="openalex")

    def _depth_cap_reached(
        self, target_depth: int, depth_node_cap: int, depth_page_cap: int
    ) -> bool:
        if target_depth != 3:
            return False
        nodes_reached = self.store.count_frontier_depth(target_depth) >= depth_node_cap
        pages = int(self.store.get_metadata(f"depth{target_depth}_pages", "0") or 0)
        return nodes_reached or pages >= depth_page_cap

    def _increment_page_count(self, target_depth: int) -> None:
        key = f"depth{target_depth}_pages"
        pages = int(self.store.get_metadata(key, "0") or 0)
        self.store.set_metadata(key, str(pages + 1))
