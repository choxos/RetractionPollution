# RetractionPollution Study Re-Audit

**Audit date:** 2026-06-30 21:07 EDT  
**Scope:** Current R port, `legacy/data/processed/opencitations.duckdb`, `outputs/opencitations/`, tests, and prior `audit/audit.md`.  
**Mode:** Audit only. No code, data, or result-generation changes were made.  
**Reviewer workflow:** Sequential fallback for the statistical-review roles. Subagent tools exist, but this session requires explicit user authorization before spawning agents.

## Bottom Line

The R rewrite fixes several defects that made the legacy Python output unusable: Retraction Watch date strings now parse, the OpenCitations result set has been analyzed, the old edge-conflict bug is fixed in storage, and the analysis hot paths are vectorized. The current outputs are much more credible than the legacy outputs.

They are still not publication-ready. The headline tables continue to operate at the Retraction Watch record level while duplicate DOI seed records remain in the database and top-seed output; the generated report promised by the code and README is absent; provenance metadata is incomplete; OpenCitations-only outputs have no topic labels and no bridge-paper titles; and there is an unexplained 7,281-edge difference between the full database edge table and exported depth-2 edge table. The study should be modified before making scientific claims from these outputs.

## Sequential Review Passes

### Intake / File Cartographer

Available evidence surfaces inspected:

- Current R package code: `R/util.R`, `R/storage.R`, `R/analysis.R`, `R/opencitations_pipeline.R`, `R/report.R`, `R/cli.R`.
- Tests: `tests/testthat/test-util.R`, `tests/testthat/test-analysis.R`, `tests/testthat/test-storage.R`.
- Prior audit: `audit/audit.md`.
- Generated outputs: `outputs/opencitations/tables/*.csv`, `outputs/opencitations/figures/*.png`, `outputs/opencitations/graphs/network_depth2.graphml`.
- Source database: `legacy/data/processed/opencitations.duckdb`.

No manuscript, protocol, SAP, reporting checklist, or response letter was present. The closest "report" surface should have been `outputs/opencitations/report.md` or `outputs/opencitations/report.html`, but neither file exists.

Study design: descriptive citation-network study using Retraction Watch seeds and OpenCitations DOI citation edges. This is not a causal design as currently implemented. A DAG is not required for the current descriptive counts, but the "pollution" framing should not be interpreted as a causal or counterfactual effect without a baseline, comparator, or explicit causal estimand.

## Prior Audit Item Status

| Prior item | Current status | Evidence |
|---|---:|---|
| CRITICAL-1, Retraction Watch `M/D/YYYY 0:00` date parsing | Fixed | `parse_date()` strips trailing time and parses `mdY` orders in `R/util.R:96`; smoke check returned `2026-01-21` for `1/21/2026 0:00`; regression tests at `tests/testthat/test-util.R:74`. |
| CRITICAL-2, no analysis on completed OpenCitations crawl | Fixed for tables/figures | `outputs/opencitations/tables/summary.csv` reports 2,655,541 nodes, 3,522,168 exported edges, and 331,643 post-notice direct citers. |
| HIGH-1, no analysis tests | Mostly fixed | `tests/testthat/test-analysis.R:120` tests seed metrics, bridge metrics, summary metrics, and end-to-end output writing. Tests pass. |
| HIGH-2/HIGH-3, first-writer edge metadata | Fixed in current storage code | `StudyStore$add_edge()` uses `ON CONFLICT` with min-depth update in `R/storage.R:274`; regression test at `tests/testthat/test-storage.R:118`. |
| HIGH-4/HIGH-5, non-vectorized seed/bridge metrics | Fixed in current R code | `seed_metrics()` and `bridge_metrics()` use joins/counts rather than per-node full-frame scans in `R/analysis.R:104` and `R/analysis.R:224`. |
| HIGH-6, no DOI seed de-duplication | Not fixed in current data/output | 62,337 top-seed rows map to 60,305 unique `openalex_id` values; 3,635 rows are duplicate-DOI seed records across 1,603 duplicate DOI groups. |
| MED-2, retracted status propagation for retracted citing works | Partially fixed | OpenCitations crawler checks whether each citing `source_id` is also a seed in `R/opencitations_pipeline.R:202`; however, duplicate DOI seed handling remains unresolved. |
| MED-6, static/nonexistent narrative report | Not fixed in generated outputs | `write_report()` exists and CLI calls it in `R/cli.R:250`, but no `outputs/opencitations/report.md` or `report.html` exists. |
| MED-7, provenance/snapshot metadata | Not fixed in current database metadata | `run_metadata` has `pipeline_mode`, `opencitations_seed_stats`, and `date_backfill_performed`, but no `rw_snapshot_date`, `oc_access_date`, `openalex_access_date`, `last_crawl_summary`, or `depth3_truncated` key in the inspected DB. |
| MED-8, no statistical comparison group/baseline | Not fixed | Outputs remain descriptive counts. No matched non-retracted controls, pre/post rate model, uncertainty intervals, or counterfactual estimand are present. |

## Major Findings

### 1. Duplicate DOI seed records still contaminate the headline per-seed outputs

Severity: Major.

The README claims that "Seeds are deduplicated by `OriginalPaperDOI` before frontier construction" (`README.md:32`). The current database and output contradict that claim.

Evidence from `legacy/data/processed/opencitations.duckdb`:

- `seeds`: 68,249 rows.
- resolved seed records: 62,337 rows.
- duplicate DOI groups: 1,603.
- duplicate seed records inside those DOI groups: 3,635.

Evidence from `outputs/opencitations/tables/top_polluted_seeds.csv`:

- rows: 62,337.
- unique `openalex_id` values: 60,305.
- duplicate `openalex_id` rows: 3,635.
- duplicate DOI groups represented in the top-seed table: 1,603.

This is not cosmetic. The same DOI appears multiple times with identical reach counts but different Retraction Watch record IDs and notice dates. For example, the top table contains both EOC and retraction rows for `doi:10.1038/nature04533`, and both rows receive the same `direct_citers`, `depth2_descendants`, and `total_depth2_reach` while post-notice counts differ by notice date. That makes "most polluted seeds" a record-level ranking, not a unique-paper ranking.

Required modification: decide the estimand explicitly. If the unit is a retracted paper, collapse seeds by cleaned `OriginalPaperDOI` before frontier construction and report retained notice logic, for example earliest EOC, retraction date, most severe notice, or a multi-notice summary. If the unit is a Retraction Watch record, the report must stop claiming DOI de-duplication and must label the output as record-level.

### 2. The exported edge count does not equal the database edge count, and the difference is not documented

Severity: Major.

The database contains 3,529,449 `citation_edges`, while `outputs/opencitations/tables/edges_depth2.csv` contains 3,522,168 rows. The difference is 7,281 edges.

Read-only DuckDB reconciliation showed:

| Quantity | Count |
|---|---:|
| Total DB edges | 3,529,449 |
| DB edges whose source is not in depth <= 2 frontier | 0 |
| DB edges whose target is not in depth <= 2 frontier | 7,281 |
| DB edges matching the export join | 3,522,168 |

This follows directly from `run_analysis()` joining both edge endpoints to `frontier_nodes` and requiring both endpoint depths to be `<= max_analysis_depth` (`R/analysis.R:41`). In the inspected examples, the dropped edges are depth-1 OpenCitations edges whose target DOI is not present in the frontier table.

This may be defensible if these are alternate cited DOI targets returned by OpenCitations and intentionally excluded from the depth-2 graph. But the current summary labels `depth2_edges` without explaining that it is an induced frontier-edge count rather than the full stored edge count. The missing 7,281 edges also mean `summary.csv` does not reconcile to the database edge table unless the export filter is known.

Required modification: either add those target nodes to the frontier consistently, or document and report the distinction between stored edges and exported induced depth-2 graph edges. The summary should include both counts or explicitly say it reports only edges whose source and target are in the depth-2 frontier.

### 3. The promised study report was not generated

Severity: Major.

The report-generation code exists. `cmd_run_opencitations()` calls `write_report()` after `run_analysis()` (`R/cli.R:247` and `R/cli.R:250`), and `write_report()` always attempts a Quarto render then writes `report.md` as fallback (`R/report.R:24` and `R/report.R:64`).

However, the inspected output directory contains tables, figures, and GraphML only. It does not contain:

- `outputs/opencitations/report.md`
- `outputs/opencitations/report.html`
- `outputs/opencitations/report.qmd`

This contradicts the README claim that "Report is a Quarto narrative with sample size, top seeds, embedded figures" (`README.md:35`). It also prevents normal abstract/body/table/figure consistency review because the actual study narrative is missing.

Required modification: run and verify the report surface, or remove the claim that a report exists. If `write_report()` failed silently in an earlier run, that failure mode also needs correction because a study pipeline should not silently ship tables without its narrative report.

### 4. Provenance metadata are incomplete in the inspected database

Severity: Major.

The current DB metadata includes:

- `pipeline_mode = opencitations`
- `opencitations_seed_stats = {"doi_seeds":62337,"loaded":68249,"no_doi":5912}`
- `date_backfill_performed = 2026-06-30 17:06:42.755324`
- 52 `opencitations_failed_parent:*` keys.

It does not include the keys that `write_report()` expects:

- `rw_snapshot_date`
- `oc_access_date`
- `openalex_access_date`
- `last_crawl_summary`
- `depth3_truncated`

`write_report()` defaults missing provenance fields to `"unknown"` (`R/report.R:30`). Therefore, even if a report were generated from this DB now, source snapshot dates would remain unknown unless the metadata are backfilled.

Required modification: store the Retraction Watch snapshot filename/date, OpenCitations access date, OpenAlex access date if used, crawl summary, truncation status, and failed-parent count as explicit report-facing metadata. This matters because citation indexes and Retraction Watch records change over time.

### 5. OpenCitations-only output cannot support topic or bridge-paper interpretation as currently exported

Severity: Major for topic/bridge claims; moderate for raw network-size claims.

`topic_distribution.csv` contains only two rows:

| depth | topic_domain | topic_name | node_count |
|---:|---|---|---:|
| 1 | NA | NA | 773,146 |
| 2 | NA | NA | 1,822,090 |

The top rows of `bridge_papers.csv` all have `title = NA`. This is expected from the current OpenCitations-only crawler because citing works are stored with `title = NA`, `topic_name = NA`, and `topic_domain = NA` (`R/opencitations_pipeline.R:204`).

As a result, the OpenCitations-only outputs can support counts and DOI-level network structure, but they cannot support claims about topical distribution or human-interpretable bridge papers without metadata enrichment.

Required modification: either enrich frontier works through OpenAlex/Crossref metadata before report generation or remove/de-emphasize topic and bridge-title claims from the OpenCitations-only report.

## Moderate Findings

### 6. The OpenCitations crawl has recorded failed parents, but summary output does not expose them

Severity: Moderate.

The database has 52 `opencitations_failed_parent:*` metadata entries. Examples include `OpenCitations HTTP 400` and `OpenCitations request failed after retries`.

The crawler records failed parents in `run_metadata` (`R/opencitations_pipeline.R:171`), but `summary.csv` does not include failed-parent count, affected depth, retry state, or percentage impact. The report would also lack this if `last_crawl_summary` remains absent.

Required modification: surface failed-parent counts and affected parent IDs in the report and summary metadata. If the failures are known-invalid DOI syntax, classify them separately from API retry exhaustion.

### 7. The current "post-notice" metric is a descriptive count, not a rate or causal estimate

Severity: Moderate.

The R port now computes nonzero post-notice direct citations: `summary.csv` reports 331,643. The implementation filters citing work publication dates after the seed notice date (`R/analysis.R:176`) and produces a timeline from direct citations to seeds (`R/analysis.R:367`).

This is useful descriptive evidence, but it is not a post-retraction citation rate, does not model time-at-risk, and does not compare against pre-notice citation rates or non-retracted controls. The study should not claim that retraction notices caused, prevented, or failed to prevent citation behavior from these counts alone.

Required modification: label this as "post-notice direct citation count" unless a rate model, time-since-notice denominator, or comparator analysis is added.

### 8. Seed selection excludes 5,912 no-DOI Retraction Watch records

Severity: Moderate.

The OpenCitations-only run loaded 68,249 seed records, resolved 62,337 DOI-bearing seeds, and marked 5,912 as `no_doi`. This is a 91.3 percent record-level resolution rate. The excluded 8.7 percent may be systematically older, non-journal, or lower-resource literature.

Required modification: disclose the no-DOI exclusion and describe the likely selection direction. If the study target is all retracted/concerning papers, consider a hybrid OpenAlex title/PMID fallback for the no-DOI group.

### 9. Tests cover regressions but not the large-output invariants

Severity: Moderate.

The test suite passed with one CRAN skip and two package-version warnings. Existing tests now cover the prior date parsing bug (`tests/testthat/test-util.R:74`), seed/bridge metrics (`tests/testthat/test-analysis.R:120` and `tests/testthat/test-analysis.R:157`), end-to-end output writing (`tests/testthat/test-analysis.R:211`), BFS frontier promotion (`tests/testthat/test-storage.R:94`), and min-depth edge merge (`tests/testthat/test-storage.R:118`).

What remains missing is a validation test or audit script for the real generated artifact set: database-to-CSV reconciliation, duplicate DOI counts, failed-parent counts, missing provenance keys, and report existence. The current test suite can pass while the output package is still not publication-ready.

Required modification: add a read-only artifact validation script or testthat context that runs against a supplied output directory and DB, separate from unit tests that use toy graphs.

## Minor Findings

- `README.md:32` claims seed de-duplication that is not true for the inspected DB/output.
- `README.md:34` claims provenance and snapshot dates are recorded in run metadata and the report, but the inspected metadata lacks the expected source snapshot keys and the report is absent.
- `README.md:35` claims a Quarto narrative report exists, but no report file exists under `outputs/opencitations/`.
- `DESCRIPTION:8` says OpenAlex is used for work resolution, recursive expansion, metadata, and graph construction, but the inspected output is OpenCitations-only with `openalex_edges = 0`. This is fine for the package capability, but the current output report needs to distinguish package design from actual run mode.
- `report.R` says "OpenAlex fills gaps" in the markdown report text (`R/report.R:196`) even though OpenCitations-only results have `openalex_edges = 0`. The report text should be mode-aware.

## Numerical Verification

### Confirmed reconciliations

- `summary.csv` depth counts match `nodes_depth2.csv`:
  - depth 0: 60,305
  - depth 1: 773,146
  - depth 2: 1,822,090
  - total: 2,655,541
- `summary.csv` edge count matches `edges_depth2.csv`: 3,522,168 rows.
- `edges_depth2.csv` has 3,522,168 unique `(source_id, target_id)` pairs.
- All exported edges in `edges_depth2.csv` have `source_api = opencitations`.
- Date parsing no longer has the old all-NULL failure: DB seeds have zero NULL notice dates and zero NULL original paper dates.

### Confirmed discrepancies or caveats

- DB `citation_edges` has 3,529,449 rows, but exported `edges_depth2.csv` has 3,522,168 rows.
- The 7,281 dropped DB edges have targets outside the depth-2 frontier join.
- `top_polluted_seeds.csv` has 62,337 rows but only 60,305 unique `openalex_id` values.
- `topic_distribution.csv` has no non-missing topic labels.
- `bridge_papers.csv` has missing titles in inspected top rows.
- `outputs/opencitations/report.md` and `outputs/opencitations/report.html` are absent.

### Verification commands run

- `Rscript -e 'library(testthat); test_dir("tests/testthat", reporter="summary")'`
  - Result: testthat completed; one CRAN skip; warnings that `duckdb` and `igraph` were built under R 4.6.1.
- Read-only DuckDB aggregate checks against `legacy/data/processed/opencitations.duckdb`.
- Read-only CSV checks against `outputs/opencitations/tables/*.csv`.
- Source-line inspection of R implementation and tests.

I did not run `R CMD check`; this was an audit of study validity and generated artifacts, not a package-release audit, and `R CMD check` would add noisy build/check artifacts.

## Cross-Document and Reporting-Checklist Checks

There is no manuscript, abstract, supplement, protocol, SAP, or reporting checklist in the repo. The available cross-document check is therefore README/code/output/database consistency.

Confirmed inconsistencies:

- README claims DOI seed de-duplication; database and output show duplicate DOI seed records remain.
- README claims provenance/snapshot recording; inspected DB metadata lacks source snapshot keys.
- README claims report generation; output directory lacks report files.
- DESCRIPTION describes OpenAlex metadata capability; current output is OpenCitations-only and has no OpenAlex enrichment.

## Final Recommendation

Recommendation: major modification before using the current outputs for any public scientific claim.

The R port is a real improvement over the legacy Python pipeline and fixes the original critical date-parsing and analysis-execution failures. But the current artifact set is still not a complete, internally documented study output. The minimum modification set should be:

1. Resolve the seed-unit decision and either de-duplicate DOI seeds or relabel the analysis as record-level.
2. Explain or fix the 7,281-edge stored/exported edge discrepancy.
3. Generate and verify the report surface.
4. Backfill and expose source snapshot/access metadata and crawl failure counts.
5. Remove or enrich topic and bridge-title outputs for OpenCitations-only mode.
6. Add an artifact-validation check that reconciles DB, CSV, figures, report, and provenance before declaring the study ready.

