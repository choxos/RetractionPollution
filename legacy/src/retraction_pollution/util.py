from __future__ import annotations

import hashlib
import json
import math
import re
from collections.abc import Iterable
from datetime import date, datetime
from pathlib import Path
from typing import Any, TypeVar

DOI_UNAVAILABLE = {"", "unavailable", "n/a", "na", "none", "null", "0"}
DOI_PATTERN = re.compile(r"(10\.\d{4,9}/\S+)", flags=re.I)
T = TypeVar("T")


def ensure_dir(path: Path) -> Path:
    path.mkdir(parents=True, exist_ok=True)
    return path


def is_missing(value: Any) -> bool:
    if value is None:
        return True
    if isinstance(value, float) and math.isnan(value):
        return True
    try:
        return bool(value != value)
    except Exception:
        return False


def text_or_none(value: Any) -> str | None:
    if is_missing(value):
        return None
    text = str(value).strip()
    return text or None


def clean_doi(value: Any) -> str | None:
    """Normalize common DOI representations to the bare lowercase DOI."""
    text = text_or_none(value)
    if text is None:
        return None
    doi = text.strip().strip(" .;,")
    if doi.lower() in DOI_UNAVAILABLE:
        return None
    doi = re.sub(r"^https?://(dx\.)?doi\.org/", "", doi, flags=re.I)
    doi = re.sub(r"^doi:\s*", "", doi, flags=re.I)
    match = DOI_PATTERN.search(doi)
    if match:
        doi = match.group(1)
    doi = doi.strip().strip(" .;,")
    if not doi or doi.lower() in DOI_UNAVAILABLE:
        return None
    return doi.lower()


def doi_url(value: Any) -> str | None:
    doi = clean_doi(value)
    return f"https://doi.org/{doi}" if doi else None


def clean_pmid(value: Any) -> str | None:
    pmid = text_or_none(value)
    if pmid is None:
        return None
    if not pmid or pmid in {"0", "0.0"} or pmid.lower() in {"unavailable", "n/a", "na"}:
        return None
    match = re.search(r"\d+", pmid)
    return match.group(0) if match else None


def parse_date(value: Any) -> str | None:
    """Return an ISO date string, handling year and year-month dates conservatively."""
    text = text_or_none(value)
    if text is None:
        return None
    if not text or text in {"0000-00-00", "0"}:
        return None
    for fmt in ("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y"):
        try:
            return datetime.strptime(text, fmt).date().isoformat()
        except ValueError:
            pass
    if re.fullmatch(r"\d{4}-\d{2}", text):
        return f"{text}-01"
    if re.fullmatch(r"\d{4}", text):
        return f"{text}-01-01"
    try:
        return date.fromisoformat(text[:10]).isoformat()
    except ValueError:
        return None


def compact_openalex_id(value: Any) -> str | None:
    text = text_or_none(value)
    if text is None:
        return None
    return text.rsplit("/", 1)[-1] if text.startswith("https://openalex.org/") else text


def full_openalex_id(value: Any) -> str | None:
    compact = compact_openalex_id(value)
    return f"https://openalex.org/{compact}" if compact else None


def json_dumps(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def stable_hash(values: Iterable[str]) -> str:
    payload = "\n".join(sorted(values)).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def chunked(items: Iterable[T], size: int) -> Iterable[list[T]]:
    batch: list[T] = []
    for item in items:
        batch.append(item)
        if len(batch) >= size:
            yield batch
            batch = []
    if batch:
        yield batch


def first_author_last_name(author_field: Any) -> str | None:
    text = text_or_none(author_field)
    if text is None:
        return None
    first_author = text.split(";")[0].strip()
    if not first_author:
        return None
    pieces = re.split(r"\s+", first_author)
    return pieces[-1] if pieces else None
