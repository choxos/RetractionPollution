from __future__ import annotations

import argparse
import json
import sys
from dataclasses import replace
from pathlib import Path

from .analysis import run_analysis
from .config import RETRACTION_WATCH_URL, Settings
from .crawler import CitationCrawler, resolve_pending_seeds
from .openalex import OpenAlexClient, OpenAlexRateLimitError
from .opencitations import OpenCitationsClient
from .opencitations_pipeline import OpenCitationsOnlyCrawler, prepare_opencitations_seeds
from .report import write_report
from .rw import download_retraction_watch, load_seed_rows
from .storage import StudyStore


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not hasattr(args, "func"):
        parser.print_help()
        return 2
    settings = Settings.from_env(args.data_dir, args.output_dir, args.db)
    try:
        return args.func(args, settings)
    except OpenAlexRateLimitError as exc:
        print(str(exc), file=sys.stderr)
        return 75
    except KeyboardInterrupt:
        print("Interrupted.", file=sys.stderr)
        return 130


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="rpollute",
        description=(
            "Trace citation pollution from Retraction Watch records through "
            "OpenCitations and OpenAlex."
        ),
    )
    parser.add_argument("--data-dir", default="data", help="Data directory, default: data")
    parser.add_argument(
        "--output-dir", default="outputs", help="Output directory, default: outputs"
    )
    parser.add_argument(
        "--db", default=None, help="DuckDB path, default: data/processed/study.duckdb"
    )
    subparsers = parser.add_subparsers(dest="command")

    fetch = subparsers.add_parser("fetch-rw", help="Download the Retraction Watch CSV")
    fetch.add_argument("--url", default=RETRACTION_WATCH_URL)
    fetch.set_defaults(func=cmd_fetch_rw)

    prepare = subparsers.add_parser("prepare-seeds", help="Load and resolve Retraction Watch seeds")
    prepare.add_argument("--csv", default=None, help="Retraction Watch CSV path")
    prepare.add_argument(
        "--no-resolve", action="store_true", help="Load seeds without OpenAlex resolution"
    )
    prepare.add_argument("--limit", type=int, default=None, help="Limit seed resolution count")
    prepare.add_argument(
        "--title-fallback",
        action="store_true",
        help="Use expensive title/author OpenAlex search after DOI and PMID resolution fail",
    )
    prepare.set_defaults(func=cmd_prepare_seeds)

    resolve = subparsers.add_parser(
        "resolve-seeds", help="Resolve already loaded seed records to OpenAlex works"
    )
    resolve.add_argument("--limit", type=int, default=None, help="Limit seed resolution count")
    resolve.add_argument(
        "--title-fallback",
        action="store_true",
        help="Use expensive title/author OpenAlex search after DOI and PMID resolution fail",
    )
    resolve.set_defaults(func=cmd_resolve_seeds)

    rate_limit = subparsers.add_parser(
        "rate-limit", help="Show the current OpenAlex API budget for OPENALEX_API_KEY"
    )
    rate_limit.set_defaults(func=cmd_rate_limit)

    prepare_oc = subparsers.add_parser(
        "prepare-opencitations",
        aliases=["prepare-oc"],
        help="Load Retraction Watch seeds as DOI nodes for an OpenCitations-only run",
    )
    prepare_oc.add_argument("--csv", default=None, help="Retraction Watch CSV path")
    prepare_oc.set_defaults(func=cmd_prepare_opencitations)

    crawl_oc = subparsers.add_parser(
        "crawl-opencitations",
        aliases=["crawl-oc"],
        help="Run or resume the OpenCitations-only DOI citation crawl",
    )
    crawl_oc.add_argument("--max-depth", type=int, default=3)
    crawl_oc.add_argument("--complete-depth", type=int, default=2)
    crawl_oc.add_argument("--depth3-node-cap", type=int, default=250_000)
    crawl_oc.add_argument("--parent-limit", type=int, default=None)
    crawl_oc.set_defaults(func=cmd_crawl_opencitations)

    crawl = subparsers.add_parser("crawl", help="Run or resume citation crawl")
    crawl.add_argument("--max-depth", type=int, default=3)
    crawl.add_argument("--complete-depth", type=int, default=2)
    crawl.add_argument("--batch-size", type=int, default=100)
    crawl.add_argument("--per-page", type=int, default=100)
    crawl.add_argument("--depth3-node-cap", type=int, default=250_000)
    crawl.add_argument("--depth3-page-cap", type=int, default=2_500)
    crawl.add_argument(
        "--no-opencitations",
        action="store_true",
        help="Disable the default OpenCitations-first citation supplement",
    )
    crawl.set_defaults(func=cmd_crawl)

    analyze = subparsers.add_parser("analyze", help="Generate tables, figures, and graph exports")
    analyze.add_argument("--max-analysis-depth", type=int, default=2)
    analyze.set_defaults(func=cmd_analyze)

    report = subparsers.add_parser("report", help="Write outputs/report.md")
    report.set_defaults(func=cmd_report)

    run_all = subparsers.add_parser("run-all", help="Fetch, prepare, crawl, analyze, and report")
    run_all.add_argument("--max-depth", type=int, default=3)
    run_all.add_argument("--complete-depth", type=int, default=2)
    run_all.add_argument("--batch-size", type=int, default=100)
    run_all.add_argument("--per-page", type=int, default=100)
    run_all.add_argument("--depth3-node-cap", type=int, default=250_000)
    run_all.add_argument("--depth3-page-cap", type=int, default=2_500)
    run_all.add_argument("--no-opencitations", action="store_true")
    run_all.add_argument(
        "--title-fallback",
        action="store_true",
        help="Use expensive title/author OpenAlex search after DOI and PMID resolution fail",
    )
    run_all.set_defaults(func=cmd_run_all)

    run_oc = subparsers.add_parser(
        "run-opencitations",
        aliases=["run-oc"],
        help="Fetch, prepare, crawl, analyze, and report using OpenCitations only",
    )
    run_oc.add_argument("--max-depth", type=int, default=3)
    run_oc.add_argument("--complete-depth", type=int, default=2)
    run_oc.add_argument("--depth3-node-cap", type=int, default=250_000)
    run_oc.add_argument("--parent-limit", type=int, default=None)
    run_oc.set_defaults(func=cmd_run_opencitations)
    return parser


def cmd_fetch_rw(args: argparse.Namespace, settings: Settings) -> int:
    path = download_retraction_watch(settings.raw_dir, url=args.url)
    print(f"Downloaded Retraction Watch CSV: {path}")
    return 0


def cmd_prepare_seeds(args: argparse.Namespace, settings: Settings) -> int:
    csv_path = Path(args.csv) if args.csv else settings.raw_dir / "retraction_watch_latest.csv"
    if not csv_path.exists():
        raise SystemExit(
            f"Retraction Watch CSV not found: {csv_path}. Run `rpollute fetch-rw` first."
        )
    seeds = load_seed_rows(csv_path)
    with StudyStore(settings.db_path) as store:
        count = store.upsert_seeds(seeds)
        print(f"Loaded {count} Retraction/Expression-of-concern seed records.")
        if not args.no_resolve:
            client = make_openalex(settings)
            stats = resolve_pending_seeds(
                store,
                client,
                limit=args.limit,
                title_fallback=args.title_fallback,
            )
            print(f"Resolved seeds with OpenAlex: {stats}")
        store.export_parquet_tables(settings.processed_dir / "parquet")
    return 0


def cmd_crawl(args: argparse.Namespace, settings: Settings) -> int:
    openalex = make_openalex(settings)
    opencitations = None if args.no_opencitations else make_opencitations(settings)
    with StudyStore(settings.db_path) as store:
        crawler = CitationCrawler(store, openalex, opencitations)
        summary = crawler.crawl(
            max_depth=args.max_depth,
            complete_depth=args.complete_depth,
            batch_size=args.batch_size,
            per_page=args.per_page,
            depth3_node_cap=args.depth3_node_cap,
            depth3_page_cap=args.depth3_page_cap,
        )
        store.export_parquet_tables(settings.processed_dir / "parquet")
    print(f"Crawl summary: {summary}")
    return 0


def cmd_prepare_opencitations(args: argparse.Namespace, settings: Settings) -> int:
    settings = opencitations_only_settings(args, settings)
    csv_path = Path(args.csv) if args.csv else settings.raw_dir / "retraction_watch_latest.csv"
    if not csv_path.exists():
        raise SystemExit(
            f"Retraction Watch CSV not found: {csv_path}. Run `rpollute fetch-rw` first."
        )
    seeds = load_seed_rows(csv_path)
    with StudyStore(settings.db_path) as store:
        stats = prepare_opencitations_seeds(store, seeds)
        store.export_parquet_tables(settings.processed_dir / "opencitations_parquet")
    print(f"Prepared OpenCitations-only seeds: {stats}")
    print(f"Database: {settings.db_path}")
    return 0


def cmd_crawl_opencitations(args: argparse.Namespace, settings: Settings) -> int:
    settings = opencitations_only_settings(args, settings)
    with StudyStore(settings.db_path) as store:
        crawler = OpenCitationsOnlyCrawler(store, make_opencitations(settings))
        summary = crawler.crawl(
            max_depth=args.max_depth,
            complete_depth=args.complete_depth,
            depth3_node_cap=args.depth3_node_cap,
            parent_limit=args.parent_limit,
        )
        store.export_parquet_tables(settings.processed_dir / "opencitations_parquet")
    print(f"OpenCitations-only crawl summary: {summary}")
    return 0


def cmd_rate_limit(args: argparse.Namespace, settings: Settings) -> int:
    print(json.dumps(make_openalex(settings).rate_limit_status(), indent=2, sort_keys=True))
    return 0


def cmd_resolve_seeds(args: argparse.Namespace, settings: Settings) -> int:
    client = make_openalex(settings)
    with StudyStore(settings.db_path) as store:
        stats = resolve_pending_seeds(
            store,
            client,
            limit=args.limit,
            title_fallback=args.title_fallback,
        )
        store.export_parquet_tables(settings.processed_dir / "parquet")
    print(f"Resolved seeds with OpenAlex: {stats}")
    return 0


def cmd_analyze(args: argparse.Namespace, settings: Settings) -> int:
    with StudyStore(settings.db_path) as store:
        summary = run_analysis(
            store, settings.output_dir, max_analysis_depth=args.max_analysis_depth
        )
    print(f"Analysis complete: {summary}")
    return 0


def cmd_report(args: argparse.Namespace, settings: Settings) -> int:
    with StudyStore(settings.db_path) as store:
        path = write_report(store, settings.output_dir)
    print(f"Report written: {path}")
    return 0


def cmd_run_opencitations(args: argparse.Namespace, settings: Settings) -> int:
    settings = opencitations_only_settings(args, settings)
    path = download_retraction_watch(settings.raw_dir)
    print(f"Downloaded Retraction Watch CSV: {path}")
    seeds = load_seed_rows(path)
    with StudyStore(settings.db_path) as store:
        seed_stats = prepare_opencitations_seeds(store, seeds)
        print(f"Prepared OpenCitations-only seeds: {seed_stats}")
        crawler = OpenCitationsOnlyCrawler(store, make_opencitations(settings))
        crawl_summary = crawler.crawl(
            max_depth=args.max_depth,
            complete_depth=args.complete_depth,
            depth3_node_cap=args.depth3_node_cap,
            parent_limit=args.parent_limit,
        )
        print(f"OpenCitations-only crawl summary: {crawl_summary}")
        store.export_parquet_tables(settings.processed_dir / "opencitations_parquet")
        analysis_summary = run_analysis(store, settings.output_dir, max_analysis_depth=2)
        print(f"Analysis complete: {analysis_summary}")
        report_path = write_report(store, settings.output_dir)
        print(f"Report written: {report_path}")
    return 0


def cmd_run_all(args: argparse.Namespace, settings: Settings) -> int:
    path = download_retraction_watch(settings.raw_dir)
    print(f"Downloaded Retraction Watch CSV: {path}")
    seeds = load_seed_rows(path)
    with StudyStore(settings.db_path) as store:
        print(f"Loaded {store.upsert_seeds(seeds)} seed records.")
        stats = resolve_pending_seeds(
            store,
            make_openalex(settings),
            title_fallback=args.title_fallback,
        )
        print(f"Resolved seeds with OpenAlex: {stats}")
        crawler = CitationCrawler(
            store,
            make_openalex(settings),
            None if args.no_opencitations else make_opencitations(settings),
        )
        crawl_summary = crawler.crawl(
            max_depth=args.max_depth,
            complete_depth=args.complete_depth,
            batch_size=args.batch_size,
            per_page=args.per_page,
            depth3_node_cap=args.depth3_node_cap,
            depth3_page_cap=args.depth3_page_cap,
        )
        print(f"Crawl summary: {crawl_summary}")
        store.export_parquet_tables(settings.processed_dir / "parquet")
        analysis_summary = run_analysis(store, settings.output_dir, max_analysis_depth=2)
        print(f"Analysis complete: {analysis_summary}")
        report_path = write_report(store, settings.output_dir)
        print(f"Report written: {report_path}")
    return 0


def make_openalex(settings: Settings) -> OpenAlexClient:
    if not settings.openalex_api_key:
        raise SystemExit(
            "OPENALEX_API_KEY is required for this workload. "
            "Set it in your shell before running collection commands."
        )
    return OpenAlexClient(
        api_key=settings.openalex_api_key,
        email=settings.openalex_email,
        request_delay=settings.openalex_request_delay,
        rate_limit_sleep=settings.openalex_rate_limit_sleep,
    )


def make_opencitations(settings: Settings) -> OpenCitationsClient:
    return OpenCitationsClient(token=settings.opencitations_token)


def opencitations_only_settings(args: argparse.Namespace, settings: Settings) -> Settings:
    db_path = settings.db_path
    output_dir = settings.output_dir
    if getattr(args, "db", None) is None:
        db_path = settings.processed_dir / "opencitations.duckdb"
    if str(getattr(args, "output_dir", "outputs")) == "outputs":
        output_dir = Path("outputs") / "opencitations"
    return replace(settings, db_path=db_path, output_dir=output_dir)


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
