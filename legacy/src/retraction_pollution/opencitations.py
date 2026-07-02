from __future__ import annotations

import re
import time
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from http.client import IncompleteRead, RemoteDisconnected
from typing import Any

from .util import clean_doi, clean_pmid, parse_date

OPENCITATIONS_INDEX_API = "https://api.opencitations.net/index/v2"
OPENCITATIONS_META_API = "https://api.opencitations.net/meta/v1"


class OpenCitationsError(RuntimeError):
    pass


@dataclass(frozen=True)
class OpenCitation:
    citing_doi: str | None
    citing_pmid: str | None
    cited_doi: str | None
    creation_date: str | None
    raw: dict[str, Any]


class OpenCitationsClient:
    def __init__(
        self,
        token: str | None = None,
        index_base_url: str = OPENCITATIONS_INDEX_API,
        meta_base_url: str = OPENCITATIONS_META_API,
        retries: int = 5,
        request_delay: float = 0.2,
    ):
        self.token = token
        self.index_base_url = index_base_url.rstrip("/")
        self.meta_base_url = meta_base_url.rstrip("/")
        self.retries = retries
        self.request_delay = request_delay

    def _request_json(self, url: str) -> Any:
        headers = {
            "Accept": "application/json",
            "User-Agent": "RetractionPollution/0.1 (OpenCitations citation supplement)",
        }
        if self.token:
            headers["authorization"] = self.token
        last_error: Exception | None = None
        for attempt in range(self.retries):
            if attempt or self.request_delay:
                time.sleep(self.request_delay if attempt == 0 else min(60.0, 2.0**attempt))
            request = urllib.request.Request(url, headers=headers)
            try:
                with urllib.request.urlopen(request, timeout=60) as response:
                    import json

                    return json.loads(response.read().decode("utf-8"))
            except urllib.error.HTTPError as exc:
                last_error = exc
                if exc.code == 404:
                    return []
                if exc.code == 429 or 500 <= exc.code < 600:
                    continue
                raise OpenCitationsError(f"OpenCitations HTTP {exc.code}") from exc
            except (IncompleteRead, RemoteDisconnected, TimeoutError) as exc:
                last_error = exc
                continue
            except urllib.error.URLError as exc:
                last_error = exc
                continue
        raise OpenCitationsError("OpenCitations request failed after retries") from last_error

    def citations_by_doi(self, doi: str) -> list[OpenCitation]:
        cleaned = clean_doi(doi)
        if not cleaned:
            return []
        url = f"{self.index_base_url}/citations/doi:{urllib.parse.quote(cleaned, safe='')}"
        data = self._request_json(url)
        if not isinstance(data, list):
            return []
        return [parse_open_citation(item) for item in data if isinstance(item, dict)]

    def metadata_by_doi(self, doi: str) -> dict[str, Any] | None:
        cleaned = clean_doi(doi)
        if not cleaned:
            return None
        url = f"{self.meta_base_url}/metadata/doi:{urllib.parse.quote(cleaned, safe='')}"
        data = self._request_json(url)
        if isinstance(data, list) and data:
            return data[0]
        return None


def doi_node_id(doi: str | None) -> str | None:
    cleaned = clean_doi(doi)
    return f"doi:{cleaned}" if cleaned else None


def pmid_node_id(pmid: str | None) -> str | None:
    cleaned = clean_pmid(pmid)
    return f"pmid:{cleaned}" if cleaned else None


def doi_from_node_id(node_id: str | None) -> str | None:
    if not node_id or not node_id.startswith("doi:"):
        return None
    return clean_doi(node_id[4:])


def extract_pid(pid_string: str | None, prefix: str) -> str | None:
    if not pid_string:
        return None
    match = re.search(rf"(?:^|\s){re.escape(prefix)}:([^\s]+)", pid_string, flags=re.I)
    return match.group(1) if match else None


def parse_open_citation(item: dict[str, Any]) -> OpenCitation:
    return OpenCitation(
        citing_doi=clean_doi(extract_pid(item.get("citing"), "doi")),
        citing_pmid=clean_pmid(extract_pid(item.get("citing"), "pmid")),
        cited_doi=clean_doi(extract_pid(item.get("cited"), "doi")),
        creation_date=parse_date(item.get("creation")),
        raw=item,
    )
