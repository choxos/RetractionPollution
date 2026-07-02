from __future__ import annotations

import csv
import shutil
import urllib.request
from collections.abc import Iterable
from datetime import UTC, datetime
from pathlib import Path

from .config import RETRACTION_WATCH_URL
from .util import (
    clean_doi,
    clean_pmid,
    ensure_dir,
    first_author_last_name,
    json_dumps,
    parse_date,
    text_or_none,
)

NOTICE_TYPES = ("retraction", "expression of concern")


def download_retraction_watch(
    raw_dir: Path,
    url: str = RETRACTION_WATCH_URL,
    filename: str | None = None,
) -> Path:
    ensure_dir(raw_dir)
    if filename is None:
        stamp = datetime.now(UTC).strftime("%Y%m%dT%H%M%SZ")
        filename = f"retraction_watch_{stamp}.csv"
    output_path = raw_dir / filename
    request = urllib.request.Request(
        url,
        headers={"User-Agent": "RetractionPollution/0.1 (research pipeline)"},
    )
    with urllib.request.urlopen(request, timeout=120) as response:
        with output_path.open("wb") as handle:
            shutil.copyfileobj(response, handle)
    latest_path = raw_dir / "retraction_watch_latest.csv"
    latest_path.write_bytes(output_path.read_bytes())
    return output_path


def iter_retraction_watch_rows(path: Path) -> Iterable[dict[str, str]]:
    # Crossref notes occasional UTF-8 issues in the Retraction Watch export. Replacement keeps
    # the row shape intact without failing a full pipeline run on one malformed name.
    with path.open("r", encoding="utf-8-sig", errors="replace", newline="") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            yield {key.strip(): (value or "").strip() for key, value in row.items() if key}


def is_seed_notice(nature: str | None) -> bool:
    text = (nature or "").lower()
    return any(notice in text for notice in NOTICE_TYPES)


def row_to_seed(row: dict[str, str]) -> dict[str, str | None]:
    doi = clean_doi(row.get("OriginalPaperDOI"))
    pmid = clean_pmid(row.get("OriginalPaperPubMedID"))
    notice_type = row.get("RetractionNature") or None
    return {
        "record_id": row.get("Record ID") or row.get("RecordID") or "",
        "title": row.get("Title") or None,
        "notice_type": notice_type,
        "notice_date": parse_date(row.get("RetractionDate")),
        "original_paper_date": parse_date(row.get("OriginalPaperDate")),
        "original_doi": doi,
        "original_pmid": pmid,
        "author": row.get("Author") or None,
        "journal": row.get("Journal") or None,
        "publisher": row.get("Publisher") or None,
        "subject": row.get("Subject") or None,
        "reason": row.get("Reason") or None,
        "article_type": row.get("ArticleType") or None,
        "country": row.get("Country") or None,
        "openalex_id": None,
        "resolved_by": None,
        "resolved_status": "pending",
        "source_row_json": json_dumps(row),
    }


def load_seed_rows(path: Path) -> list[dict[str, str | None]]:
    seeds: list[dict[str, str | None]] = []
    for row in iter_retraction_watch_rows(path):
        if not is_seed_notice(row.get("RetractionNature")):
            continue
        seed = row_to_seed(row)
        if seed["record_id"]:
            seeds.append(seed)
    return seeds


def search_fallback_terms(seed: dict[str, str | None]) -> tuple[str | None, str | None]:
    return text_or_none(seed.get("title")), first_author_last_name(seed.get("author"))
