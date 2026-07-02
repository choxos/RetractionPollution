# RetractionPollution

Trace how retracted articles and expressions of concern continue to influence the
scientific literature through citation descendants.

The pipeline uses:

- **Retraction Watch / Crossref** for seed records.
- **OpenCitations** first at every citation depth for DOI-based citation lookup,
  matching the source order used in `XeraRetractionTracker`.
- **OpenAlex** for work resolution, recursive citation expansion, metadata, graph
  construction, and filling gaps where OpenCitations cannot resolve a parent.

Headline analysis is designed for a complete depth-2 network. Depth 3 is collected
as a capped export so the run remains API- and storage-aware.

## Setup

```bash
uv sync
export OPENALEX_API_KEY="your-openalex-key"
export OPENALEX_EMAIL="you@example.org"        # optional polite-pool contact
export OPENALEX_REQUEST_DELAY="0.35"           # optional seconds between OpenAlex requests
export OPENCITATIONS_TOKEN="your-token"        # optional
```

Do not commit API keys. The project reads secrets only from environment variables.

## Quick Start

```bash
rpollute fetch-rw &&
rpollute prepare-seeds &&
rpollute crawl &&
rpollute analyze &&
rpollute report
```

Or run the full workflow:

```bash
rpollute run-all
```

## OpenCitations-Only Workflow

To avoid OpenAlex entirely, use the OpenCitations-only commands. This builds a
DOI-based graph using OpenCitations Index citation records and stores it in a
separate database by default.

```bash
uv run rpollute run-opencitations
```

Equivalent step-by-step run:

```bash
uv run rpollute fetch-rw &&
uv run rpollute prepare-opencitations &&
uv run rpollute crawl-opencitations &&
uv run rpollute analyze --db data/processed/opencitations.duckdb --output-dir outputs/opencitations &&
uv run rpollute report --db data/processed/opencitations.duckdb --output-dir outputs/opencitations
```

OpenCitations-only defaults:

- No `OPENALEX_API_KEY` is required or used.
- Only Retraction Watch seed records with `OriginalPaperDOI` can become seed
  nodes.
- Citation descendants without DOI are retained as PMID leaf nodes when PMID is
  available, but recursive traversal follows DOI nodes only.
- Outputs go to `outputs/opencitations/` and the database is
  `data/processed/opencitations.duckdb` unless overridden.

Default crawl behavior:

- Retraction Watch seed filter: `Retraction` and `Expression of concern`.
- OpenCitations is attempted first for DOI-bearing parent nodes at depths 0, 1,
  and 2.
- OpenAlex resolves all works into a consistent OpenAlex ID graph and supplements
  citation discovery.
- Seed resolution uses DOI and PMID by default. Title/author fallback is available
  with `--title-fallback`, but it is intentionally opt-in because it can require
  many thousands of OpenAlex search calls.
- Depth 1 and depth 2 are complete by default.
- Depth 3 is capped at `250000` nodes or `2500` OpenAlex pages.

## Outputs

Generated artifacts are intentionally ignored by Git:

- `data/raw/`: downloaded Retraction Watch snapshots.
- `data/processed/study.duckdb`: crawl state and analysis database.
- `data/processed/parquet/`: Parquet exports of core tables.
- `outputs/tables/`: summary tables and network node/edge CSVs.
- `outputs/figures/`: PNG/SVG figures.
- `outputs/graphs/network_depth2.graphml`: graph export for Gephi, Cytoscape,
  or other network tools.
- `outputs/report.md`: concise generated report.

## Useful Commands

```bash
# Load seeds without resolving them yet
rpollute prepare-seeds --no-resolve

# Resolve already loaded seeds without redownloading/reloading Retraction Watch
rpollute resolve-seeds &&
rpollute crawl &&
rpollute analyze &&
rpollute report

# Expensive fallback for seeds not resolved by DOI/PMID
rpollute resolve-seeds --title-fallback

# Check current OpenAlex API budget before a long run
rpollute rate-limit

# Resume crawl with smaller depth-3 caps
rpollute crawl --depth3-node-cap 50000 --depth3-page-cap 500

# Disable OpenCitations and use OpenAlex only
rpollute crawl --no-opencitations

# Re-run only the publication-oriented outputs
rpollute analyze
rpollute report
```

## Notes

OpenCitations is excellent for DOI-based direct citation discovery, but recursive
network construction still needs OpenAlex because each node must be resolved to a
stable work ID and many descendants will only be available through OpenAlex.
