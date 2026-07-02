from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

RETRACTION_WATCH_URL = (
    "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/"
    "retraction_watch.csv?ref_type=heads&inline=false"
)

DEFAULT_DATA_DIR = Path("data")
DEFAULT_OUTPUT_DIR = Path("outputs")
DEFAULT_DB_PATH = DEFAULT_DATA_DIR / "processed" / "study.duckdb"


@dataclass(frozen=True)
class Settings:
    data_dir: Path = DEFAULT_DATA_DIR
    output_dir: Path = DEFAULT_OUTPUT_DIR
    db_path: Path = DEFAULT_DB_PATH
    openalex_api_key: str | None = None
    openalex_email: str | None = None
    openalex_request_delay: float = 0.35
    openalex_rate_limit_sleep: float = 60.0
    opencitations_token: str | None = None

    @classmethod
    def from_env(
        cls,
        data_dir: str | Path = DEFAULT_DATA_DIR,
        output_dir: str | Path = DEFAULT_OUTPUT_DIR,
        db_path: str | Path | None = None,
    ) -> Settings:
        data_dir = Path(data_dir)
        output_dir = Path(output_dir)
        return cls(
            data_dir=data_dir,
            output_dir=output_dir,
            db_path=Path(db_path) if db_path else data_dir / "processed" / "study.duckdb",
            openalex_api_key=os.getenv("OPENALEX_API_KEY") or None,
            openalex_email=os.getenv("OPENALEX_EMAIL") or None,
            openalex_request_delay=float(os.getenv("OPENALEX_REQUEST_DELAY", "0.35")),
            openalex_rate_limit_sleep=float(os.getenv("OPENALEX_RATE_LIMIT_SLEEP", "60")),
            opencitations_token=os.getenv("OPENCITATIONS_TOKEN") or None,
        )

    @property
    def raw_dir(self) -> Path:
        return self.data_dir / "raw"

    @property
    def processed_dir(self) -> Path:
        return self.data_dir / "processed"

    @property
    def figure_dir(self) -> Path:
        return self.output_dir / "figures"

    @property
    def table_dir(self) -> Path:
        return self.output_dir / "tables"

    @property
    def graph_dir(self) -> Path:
        return self.output_dir / "graphs"
