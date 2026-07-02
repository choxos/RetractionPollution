from __future__ import annotations

from collections.abc import Iterable
from pathlib import Path
from typing import Any

from .util import ensure_dir, json_dumps


def _duckdb():
    try:
        import duckdb
    except ImportError as exc:  # pragma: no cover - exercised only without deps installed
        raise RuntimeError(
            "DuckDB is required for this command. Run `uv sync` or install project dependencies."
        ) from exc
    return duckdb


class StudyStore:
    def __init__(self, db_path: Path | str):
        self.db_path = Path(db_path)
        ensure_dir(self.db_path.parent)
        self.con = _duckdb().connect(str(self.db_path))
        self.init_schema()

    def close(self) -> None:
        self.con.close()

    def __enter__(self) -> StudyStore:
        return self

    def __exit__(self, *_: object) -> None:
        self.close()

    def init_schema(self) -> None:
        self.con.execute(
            """
            CREATE TABLE IF NOT EXISTS seeds (
                record_id TEXT PRIMARY KEY,
                title TEXT,
                notice_type TEXT,
                notice_date DATE,
                original_paper_date DATE,
                original_doi TEXT,
                original_pmid TEXT,
                author TEXT,
                journal TEXT,
                publisher TEXT,
                subject TEXT,
                reason TEXT,
                article_type TEXT,
                country TEXT,
                openalex_id TEXT,
                resolved_by TEXT,
                resolved_status TEXT,
                source_row_json TEXT,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """
        )
        self.con.execute(
            """
            CREATE TABLE IF NOT EXISTS works (
                openalex_id TEXT PRIMARY KEY,
                doi TEXT,
                title TEXT,
                publication_date DATE,
                publication_year INTEGER,
                work_type TEXT,
                is_retracted BOOLEAN,
                cited_by_count INTEGER,
                source_id TEXT,
                source_name TEXT,
                topic_id TEXT,
                topic_name TEXT,
                topic_domain TEXT,
                referenced_works_json TEXT,
                raw_json TEXT,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """
        )
        self.con.execute(
            """
            CREATE TABLE IF NOT EXISTS frontier_nodes (
                openalex_id TEXT PRIMARY KEY,
                depth INTEGER NOT NULL,
                processed_at TIMESTAMP,
                added_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """
        )
        self.con.execute(
            """
            CREATE TABLE IF NOT EXISTS citation_edges (
                source_id TEXT NOT NULL,
                target_id TEXT NOT NULL,
                depth INTEGER NOT NULL,
                source_api TEXT NOT NULL DEFAULT 'openalex',
                citation_date DATE,
                discovered_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                PRIMARY KEY (source_id, target_id)
            )
            """
        )
        self.con.execute(
            """
            CREATE TABLE IF NOT EXISTS crawl_jobs (
                job_id TEXT PRIMARY KEY,
                depth INTEGER NOT NULL,
                parent_ids_json TEXT NOT NULL,
                cursor TEXT NOT NULL,
                done BOOLEAN NOT NULL DEFAULT FALSE,
                pages_fetched INTEGER NOT NULL DEFAULT 0,
                results_fetched INTEGER NOT NULL DEFAULT 0,
                error TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """
        )
        self.con.execute(
            """
            CREATE TABLE IF NOT EXISTS run_metadata (
                key TEXT PRIMARY KEY,
                value TEXT,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """
        )

    def upsert_seed(self, seed: dict[str, Any]) -> None:
        columns = [
            "record_id",
            "title",
            "notice_type",
            "notice_date",
            "original_paper_date",
            "original_doi",
            "original_pmid",
            "author",
            "journal",
            "publisher",
            "subject",
            "reason",
            "article_type",
            "country",
            "openalex_id",
            "resolved_by",
            "resolved_status",
            "source_row_json",
        ]
        values = [seed.get(col) for col in columns]
        placeholders = ",".join(["?"] * len(columns))
        update_clause = ",".join([f"{col}=excluded.{col}" for col in columns[1:]])
        self.con.execute(
            f"""
            INSERT INTO seeds ({",".join(columns)})
            VALUES ({placeholders})
            ON CONFLICT(record_id) DO UPDATE SET
                {update_clause},
                updated_at=now()
            """,
            values,
        )
        if seed.get("openalex_id"):
            self.add_frontier_node(seed["openalex_id"], depth=0)

    def upsert_seeds(self, seeds: Iterable[dict[str, Any]]) -> int:
        count = 0
        for seed in seeds:
            self.upsert_seed(seed)
            count += 1
        return count

    def update_seed_resolution(
        self, record_id: str, openalex_id: str | None, resolved_by: str, status: str
    ) -> None:
        self.con.execute(
            """
            UPDATE seeds
            SET openalex_id=?, resolved_by=?, resolved_status=?, updated_at=now()
            WHERE record_id=?
            """,
            [openalex_id, resolved_by, status, record_id],
        )
        if openalex_id:
            self.add_frontier_node(openalex_id, depth=0)

    def unresolved_seeds(
        self,
        limit: int | None = None,
        *,
        include_pending_title_fallback: bool = True,
    ) -> list[dict[str, Any]]:
        sql = """
            SELECT *
            FROM seeds
            WHERE openalex_id IS NULL
              AND (resolved_status IS NULL OR resolved_status != 'not_found')
        """
        if not include_pending_title_fallback:
            sql += """
              AND (resolved_status IS NULL OR resolved_status != 'pending_title_fallback')
            """
        sql += " ORDER BY record_id"
        if limit is not None:
            sql += f" LIMIT {int(limit)}"
        return self.con.execute(sql).fetchdf().to_dict("records")

    def resolved_seed_ids(self) -> list[str]:
        rows = self.con.execute(
            """
            SELECT DISTINCT openalex_id
            FROM seeds
            WHERE openalex_id IS NOT NULL
            ORDER BY openalex_id
            """
        ).fetchall()
        return [row[0] for row in rows]

    def resolved_seeds_with_doi(self) -> list[dict[str, Any]]:
        df = self.con.execute(
            """
            SELECT record_id, title, original_doi, original_pmid, openalex_id, notice_date
            FROM seeds
            WHERE openalex_id IS NOT NULL AND original_doi IS NOT NULL
            ORDER BY record_id
            """
        ).fetchdf()
        return df.to_dict("records")

    def frontier_with_doi(self, depth: int) -> list[dict[str, Any]]:
        df = self.con.execute(
            """
            SELECT
                f.openalex_id,
                COALESCE(w.doi, s.original_doi) AS doi
            FROM frontier_nodes f
            LEFT JOIN works w ON f.openalex_id = w.openalex_id
            LEFT JOIN seeds s ON f.openalex_id = s.openalex_id
            WHERE f.depth = ?
              AND COALESCE(w.doi, s.original_doi) IS NOT NULL
              AND f.processed_at IS NULL
            ORDER BY f.openalex_id
            """,
            [depth],
        ).fetchdf()
        return df.to_dict("records")

    def upsert_work(self, work: dict[str, Any]) -> None:
        columns = [
            "openalex_id",
            "doi",
            "title",
            "publication_date",
            "publication_year",
            "work_type",
            "is_retracted",
            "cited_by_count",
            "source_id",
            "source_name",
            "topic_id",
            "topic_name",
            "topic_domain",
            "referenced_works_json",
            "raw_json",
        ]
        values = [work.get(col) for col in columns]
        placeholders = ",".join(["?"] * len(columns))
        update_clause = ",".join([f"{col}=excluded.{col}" for col in columns[1:]])
        self.con.execute(
            f"""
            INSERT INTO works ({",".join(columns)})
            VALUES ({placeholders})
            ON CONFLICT(openalex_id) DO UPDATE SET
                {update_clause},
                updated_at=now()
            """,
            values,
        )

    def add_frontier_node(self, openalex_id: str, depth: int) -> None:
        existing = self.con.execute(
            "SELECT depth FROM frontier_nodes WHERE openalex_id=?", [openalex_id]
        ).fetchone()
        if existing is None:
            self.con.execute(
                "INSERT INTO frontier_nodes (openalex_id, depth) VALUES (?, ?)",
                [openalex_id, depth],
            )
        elif depth < existing[0]:
            self.con.execute(
                "UPDATE frontier_nodes SET depth=? WHERE openalex_id=?", [depth, openalex_id]
            )

    def add_edge(
        self,
        source_id: str,
        target_id: str,
        depth: int,
        *,
        source_api: str = "openalex",
        citation_date: str | None = None,
    ) -> None:
        self.con.execute(
            """
            INSERT OR IGNORE INTO citation_edges
                (source_id, target_id, depth, source_api, citation_date)
            VALUES (?, ?, ?, ?, ?)
            """,
            [source_id, target_id, depth, source_api, citation_date],
        )

    def pending_frontier(self, depth: int, limit: int | None = None) -> list[str]:
        sql = """
            SELECT openalex_id
            FROM frontier_nodes
            WHERE depth=? AND processed_at IS NULL
            ORDER BY openalex_id
        """
        if limit is not None:
            sql += f" LIMIT {int(limit)}"
        rows = self.con.execute(sql, [depth]).fetchall()
        return [row[0] for row in rows]

    def mark_processed(self, parent_ids: Iterable[str]) -> None:
        for openalex_id in parent_ids:
            self.con.execute(
                "UPDATE frontier_nodes SET processed_at=now() WHERE openalex_id=?",
                [openalex_id],
            )

    def get_or_create_job(self, job_id: str, depth: int, parent_ids: list[str]) -> dict[str, Any]:
        row = self.con.execute("SELECT * FROM crawl_jobs WHERE job_id=?", [job_id]).fetchone()
        if row is None:
            self.con.execute(
                """
                INSERT INTO crawl_jobs (job_id, depth, parent_ids_json, cursor)
                VALUES (?, ?, ?, '*')
                """,
                [job_id, depth, json_dumps(parent_ids)],
            )
        return self.get_job(job_id)

    def get_job(self, job_id: str) -> dict[str, Any]:
        df = self.con.execute("SELECT * FROM crawl_jobs WHERE job_id=?", [job_id]).fetchdf()
        if df.empty:
            raise KeyError(job_id)
        return df.to_dict("records")[0]

    def update_job(
        self,
        job_id: str,
        *,
        cursor: str | None = None,
        done: bool | None = None,
        pages_delta: int = 0,
        results_delta: int = 0,
        error: str | None = None,
    ) -> None:
        job = self.get_job(job_id)
        self.con.execute(
            """
            UPDATE crawl_jobs
            SET cursor=?,
                done=?,
                pages_fetched=?,
                results_fetched=?,
                error=?,
                updated_at=now()
            WHERE job_id=?
            """,
            [
                cursor if cursor is not None else job["cursor"],
                done if done is not None else job["done"],
                int(job["pages_fetched"]) + pages_delta,
                int(job["results_fetched"]) + results_delta,
                error,
                job_id,
            ],
        )

    def set_metadata(self, key: str, value: Any) -> None:
        self.con.execute(
            """
            INSERT INTO run_metadata (key, value)
            VALUES (?, ?)
            ON CONFLICT(key) DO UPDATE SET value=excluded.value, updated_at=now()
            """,
            [key, json_dumps(value) if not isinstance(value, str) else value],
        )

    def get_metadata(self, key: str, default: Any = None) -> Any:
        row = self.con.execute("SELECT value FROM run_metadata WHERE key=?", [key]).fetchone()
        return row[0] if row else default

    def count_frontier_depth(self, depth: int) -> int:
        return self.con.execute(
            "SELECT COUNT(*) FROM frontier_nodes WHERE depth=?", [depth]
        ).fetchone()[0]

    def export_parquet_tables(self, out_dir: Path) -> None:
        ensure_dir(out_dir)
        for table in ("seeds", "works", "frontier_nodes", "citation_edges", "crawl_jobs"):
            self.con.execute(
                f"COPY (SELECT * FROM {table}) TO ? (FORMAT PARQUET)",
                [str(out_dir / f"{table}.parquet")],
            )
