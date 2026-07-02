# retractionpollution (R port)

R rewrite of the RetractionPollution pipeline. See `audit/audit.md` and
`audit/study-audit-2026-06-30.md` for the audits that motivated the rewrite.
This port fixes the critical date-parsing bug, canonicalizes duplicate DOI seed
records, vectorizes the metrics, writes report/validation artifacts, and ships
with tests that round-trip real Retraction Watch rows.

The original Python implementation is preserved under `legacy/`.

## Install (dev)

```r
pak::pak_local(".")
# or
install.packages(".", repos = NULL, type = "source")
```

## Run

```bash
OPENALEX_API_KEY=... Rscript -e 'retractionpollution::run_all()'
# OpenCitations-only (no OpenAlex key needed):
Rscript -e 'retractionpollution::run_opencitations()'
```

## Manuscript (reproducible)

Every headline number and every manuscript figure is generated from the study
database and the analysis tables by committed code (`R/manuscript.R` +
`inst/manuscript/manuscript.qmd`); no value is hand-entered. `run_opencitations()`
and `run_all()` build the manuscript at the end of the pipeline, or rebuild it
from an existing database:

```bash
Rscript -e 'retractionpollution::manuscript(db = "legacy/data/processed/opencitations.duckdb", output_dir = "outputs")'
```

This writes `outputs/opencitations/manuscript_stats.json` (all computed values),
the six `manuscript_fig*` figures, and renders `manuscript.{md,html,pdf,docx}`.
Post-notice totals are reported as counts of **distinct** papers alongside the
per-seed relationship sums, and the network is summarized with quantitative
reach-survival, concentration (Lorenz/Gini), and notice-aligned event-study
figures rather than an uninterpretable full-graph rendering.

## Audit-driven fixes (vs. legacy Python)

1. **`parse_date` handles `"M/D/YYYY 0:00"`** (CRITICAL-1).
2. **Analysis is run end-to-end on the OpenCitations-only crawl and emits artifact validation** (CRITICAL-2).
3. **`add_edge` uses a min-depth merge** instead of `INSERT OR IGNORE` (HIGH-2).
4. **`_seed_metrics` and `_bridge_metrics` are vectorized** dplyr joins (HIGH-4, HIGH-5).
5. **Seeds are canonicalized by `OriginalPaperDOI`** before frontier construction; duplicate Retraction Watch records stay linked as `duplicate_doi` provenance rows (HIGH-6).
6. **`is_retracted` is propagated** from seeds into `works` for any work that is also a seed (MED-2).
7. **Provenance + snapshot dates are recorded** in run metadata and in the report (MED-7).
8. **Report is a Quarto/Markdown narrative** with sample size, top seeds, embedded figures, and `tables/artifact_validation.csv` (MED-6).
9. **Analysis tests round-trip a real RW row** through parse -> seed -> metrics (HIGH-1).
