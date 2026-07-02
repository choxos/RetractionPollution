from __future__ import annotations

import time
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from typing import Any

from .util import (
    clean_doi,
    compact_openalex_id,
    doi_url,
    full_openalex_id,
    json_dumps,
    text_or_none,
)

OPENALEX_API = "https://api.openalex.org"

WORK_SELECT = ",".join(
    [
        "id",
        "doi",
        "display_name",
        "title",
        "publication_date",
        "publication_year",
        "type",
        "type_crossref",
        "is_retracted",
        "cited_by_count",
        "referenced_works",
        "primary_location",
        "primary_topic",
        "topics",
    ]
)

RESOLUTION_SELECT = ",".join(
    [
        "id",
        "doi",
        "display_name",
        "title",
        "publication_date",
        "publication_year",
        "type",
        "is_retracted",
        "cited_by_count",
        "referenced_works",
        "primary_location",
        "primary_topic",
        "topics",
        "authorships",
    ]
)


class OpenAlexError(RuntimeError):
    def __init__(
        self,
        message: str,
        *,
        status_code: int | None = None,
        response_text: str | None = None,
    ):
        super().__init__(message)
        self.status_code = status_code
        self.response_text = response_text


class OpenAlexRateLimitError(OpenAlexError):
    pass


@dataclass
class OpenAlexPage:
    results: list[dict[str, Any]]
    next_cursor: str | None
    count: int | None


class OpenAlexClient:
    def __init__(
        self,
        api_key: str | None = None,
        email: str | None = None,
        base_url: str = OPENALEX_API,
        retries: int = 6,
        request_delay: float = 0.35,
        rate_limit_sleep: float = 60.0,
    ):
        self.api_key = api_key
        self.email = email
        self.base_url = base_url.rstrip("/")
        self.retries = retries
        self.request_delay = request_delay
        self.rate_limit_sleep = rate_limit_sleep

    def _request_json(self, path: str, params: dict[str, Any] | None = None) -> dict[str, Any]:
        query: dict[str, Any] = dict(params or {})
        if self.api_key:
            query["api_key"] = self.api_key
        if self.email:
            query["mailto"] = self.email
        url = f"{self.base_url}{path}"
        if query:
            url = f"{url}?{urllib.parse.urlencode(query, doseq=True)}"

        last_error: Exception | None = None
        for attempt in range(self.retries):
            if attempt or self.request_delay:
                time.sleep(self.request_delay if attempt == 0 else min(60.0, 2.0**attempt))
            request = urllib.request.Request(
                url,
                headers={
                    "Accept": "application/json",
                    "User-Agent": "RetractionPollution/0.1 (mailto optional; OpenAlex research)",
                },
            )
            try:
                with urllib.request.urlopen(request, timeout=60) as response:
                    import json

                    return json.loads(response.read().decode("utf-8"))
            except urllib.error.HTTPError as exc:
                last_error = exc
                if exc.code == 404:
                    return {}
                if exc.code == 429:
                    retry_after = exc.headers.get("Retry-After")
                    time.sleep(_retry_after_seconds(retry_after, self.rate_limit_sleep, attempt))
                    continue
                if 500 <= exc.code < 600:
                    continue
                response_text = _read_http_error_body(exc)
                raise OpenAlexError(
                    f"OpenAlex HTTP {exc.code} for {path}: {response_text[:300]}",
                    status_code=exc.code,
                    response_text=response_text,
                ) from exc
            except urllib.error.URLError as exc:
                last_error = exc
                continue
        if isinstance(last_error, urllib.error.HTTPError) and last_error.code == 429:
            raise OpenAlexRateLimitError(
                "OpenAlex rate limit persisted after retries. The crawl state is saved; "
                "rerun the same command later to continue.",
                status_code=429,
                response_text=_read_http_error_body(last_error),
            ) from last_error
        raise OpenAlexError(f"OpenAlex request failed after {self.retries} attempts: {path}") from (
            last_error
        )

    def get_works_by_dois(self, dois: list[str]) -> list[dict[str, Any]]:
        values = list(dict.fromkeys(_openalex_doi_url(doi) for doi in dois))
        values = [value for value in values if value]
        if not values:
            return []
        if len(values) > 100:
            works: list[dict[str, Any]] = []
            for start in range(0, len(values), 100):
                works.extend(self._get_works_by_doi_values(values[start : start + 100]))
            return works
        return self._get_works_by_doi_values(values)

    def rate_limit_status(self) -> dict[str, Any]:
        return self._request_json("/rate-limit")

    def _get_works_by_doi_values(self, values: list[str]) -> list[dict[str, Any]]:
        if not values:
            return []
        try:
            data = self._request_json(
                "/works",
                {
                    "filter": "doi:" + "|".join(values),
                    "per-page": max(1, min(100, len(values))),
                    "select": RESOLUTION_SELECT,
                },
            )
            return data.get("results") or []
        except OpenAlexError as exc:
            if exc.status_code != 400:
                raise
            if len(values) == 1:
                return []
            midpoint = len(values) // 2
            return self._get_works_by_doi_values(
                values[:midpoint]
            ) + self._get_works_by_doi_values(values[midpoint:])

    def get_work_by_pmid(self, pmid: str) -> dict[str, Any] | None:
        data = self._request_json(
            f"/works/pmid:{urllib.parse.quote(str(pmid), safe='')}",
            {"select": RESOLUTION_SELECT},
        )
        return data if data.get("id") else None

    def search_work(self, title: Any, author_last_name: str | None = None) -> dict[str, Any] | None:
        title_text = text_or_none(title)
        if not title_text:
            return None
        data = self._request_json(
            "/works",
            {
                "search": title_text[:200],
                "per-page": 5,
                "select": RESOLUTION_SELECT,
            },
        )
        candidates = data.get("results") or []
        if not candidates:
            return None
        if author_last_name:
            needle = author_last_name.lower()
            for work in candidates:
                authorships = work.get("authorships") or []
                names = [
                    (authorship.get("author") or {}).get("display_name", "").lower()
                    for authorship in authorships
                ]
                if any(needle in name for name in names):
                    return work
        return candidates[0]

    def list_citers(
        self,
        parent_ids: list[str],
        *,
        cursor: str = "*",
        per_page: int = 100,
    ) -> OpenAlexPage:
        compact_ids = [compact_openalex_id(parent_id) for parent_id in parent_ids]
        compact_ids = [parent_id for parent_id in compact_ids if parent_id]
        if not compact_ids:
            return OpenAlexPage([], None, 0)
        if len(compact_ids) > 100:
            raise ValueError("OpenAlex OR filters support at most 100 values per request.")
        data = self._request_json(
            "/works",
            {
                "filter": "cites:" + "|".join(compact_ids),
                "per-page": max(1, min(100, per_page)),
                "cursor": cursor,
                "select": WORK_SELECT,
            },
        )
        meta = data.get("meta") or {}
        return OpenAlexPage(
            results=data.get("results") or [],
            next_cursor=meta.get("next_cursor"),
            count=meta.get("count"),
        )


def normalize_work(work: dict[str, Any]) -> dict[str, Any]:
    openalex_id = compact_openalex_id(work.get("id"))
    primary_location = work.get("primary_location") or {}
    source = primary_location.get("source") or {}
    primary_topic = work.get("primary_topic") or {}
    topic_domain = primary_topic.get("domain") or {}
    referenced = [compact_openalex_id(item) for item in (work.get("referenced_works") or [])]
    referenced = [item for item in referenced if item]
    return {
        "openalex_id": openalex_id,
        "doi": clean_doi(work.get("doi")),
        "title": work.get("display_name") or work.get("title"),
        "publication_date": work.get("publication_date"),
        "publication_year": work.get("publication_year"),
        "work_type": work.get("type") or work.get("type_crossref"),
        "is_retracted": work.get("is_retracted"),
        "cited_by_count": work.get("cited_by_count"),
        "source_id": compact_openalex_id(source.get("id")),
        "source_name": source.get("display_name"),
        "topic_id": compact_openalex_id(primary_topic.get("id")),
        "topic_name": primary_topic.get("display_name"),
        "topic_domain": topic_domain.get("display_name"),
        "referenced_works_json": json_dumps(referenced),
        "raw_json": json_dumps(work),
    }


def edges_from_work(work: dict[str, Any], parent_ids: set[str]) -> list[tuple[str, str]]:
    source_id = compact_openalex_id(work.get("id"))
    if not source_id:
        return []
    referenced = {compact_openalex_id(item) for item in (work.get("referenced_works") or [])}
    referenced.discard(None)
    parent_ids = {compact_openalex_id(item) for item in parent_ids}
    return [(source_id, target_id) for target_id in sorted(referenced.intersection(parent_ids))]


def openalex_url(openalex_id: str | None) -> str | None:
    return full_openalex_id(openalex_id)


def _openalex_doi_url(value: Any) -> str | None:
    doi = clean_doi(value)
    if not doi or "," in doi or "&" in doi:
        return None
    return doi_url(doi)


def _read_http_error_body(exc: urllib.error.HTTPError) -> str:
    try:
        body = exc.read()
    except Exception:
        return ""
    try:
        return body.decode("utf-8", errors="replace")
    except Exception:
        return ""


def _retry_after_seconds(
    retry_after: str | None,
    default_sleep: float,
    attempt: int,
) -> float:
    if retry_after:
        try:
            return min(300.0, max(1.0, float(retry_after)))
        except ValueError:
            pass
    return min(300.0, max(default_sleep, 10.0 * (2.0**attempt)))
