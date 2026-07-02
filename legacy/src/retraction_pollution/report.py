from __future__ import annotations

from datetime import UTC, datetime
from pathlib import Path

from .storage import StudyStore
from .util import ensure_dir


def write_report(store: StudyStore, output_dir: Path) -> Path:
    table_dir = output_dir / "tables"
    figure_dir = output_dir / "figures"
    graph_dir = output_dir / "graphs"
    ensure_dir(output_dir)

    summary = _read_summary(table_dir / "summary.csv")
    crawl_summary = store.get_metadata("last_crawl_summary", "{}")
    depth3_truncated = store.get_metadata("depth3_truncated", "false")

    lines = [
        "# Retraction Pollution Study Report",
        "",
        f"Generated: {datetime.now(UTC).isoformat()}",
        "",
        "## Summary Metrics",
        "",
    ]
    if summary:
        lines.extend(f"- **{metric}**: {value}" for metric, value in summary.items())
    else:
        lines.append("- No analysis summary found. Run `rpollute analyze` first.")

    lines.extend(
        [
            "",
            "## Generated Artifacts",
            "",
            f"- Tables: `{table_dir}`",
            f"- Figures: `{figure_dir}`",
            f"- Graph exports: `{graph_dir}`",
            "- Main graph: `outputs/graphs/network_depth2.graphml`",
            "",
            "## Collection Notes",
            "",
            "- Retraction Watch seeds include `Retraction` and `Expression of concern` notices.",
            "- OpenCitations is tried first at each citation depth for DOI-bearing parent nodes.",
            "- OpenAlex resolves works and supplements recursive citation discovery.",
            "- Headline analysis is restricted to the complete depth-2 graph.",
            f"- Depth-3 truncation flag: `{depth3_truncated}`",
            "",
            "## Last Crawl Metadata",
            "",
            "```json",
            crawl_summary,
            "```",
            "",
        ]
    )
    report_path = output_dir / "report.md"
    report_path.write_text("\n".join(lines), encoding="utf-8")
    return report_path


def _read_summary(path: Path) -> dict[str, str]:
    if not path.exists():
        return {}
    import csv

    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        return {row["metric"]: row["value"] for row in reader}
