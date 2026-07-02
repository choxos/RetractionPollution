# RetractionPollution — Code & Methodology Audit

**Auditor:** opencode (read-only review)
**Date:** 2026-06-30
**Scope:** `legacy/` Python implementation — code correctness, methodological soundness, test coverage, and reproducibility.
**Verdict:** **Not 100% sound.** One critical bug invalidates the headline analysis; several methodological gaps and untested code paths remain. Details below.

---

## 1. Study design (what the pipeline claims to do)

Trace how retracted papers and expressions-of-concern (EOC) continue to pollute the scientific literature via citation descendants.

- **Seeds:** Retraction Watch records filtered to `Retraction` and `Expression of concern` notice types.
- **Citation graph:** Built outward from seeds to depth 2 (complete) and depth 3 (capped). Two pipelines:
  1. OpenAlex + OpenCitations hybrid (DOI/PMID/title resolution → OpenAlex `cites:` filter pagination → OpenCitations supplement).
  2. OpenCitations-only (DOI-based, no OpenAlex dependency).
- **Headline metrics:** depth counts, "most polluted seeds" by depth-2 reach, post-notice direct citers, post-notice depth-2 descendants, bridge papers, topic distribution, weak components.
- **Outputs:** CSV tables, PNG/SVG figures, GraphML network, `report.md`.

The design is reasonable and the data sources (Retraction Watch, OpenCitations, OpenAlex) are the standard choices for this kind of study. The methodological concerns are in *execution*, not *conception*.

---

## 2. Critical findings (block the headline result)

### CRITICAL-1: `parse_date` cannot parse the Retraction Watch date format → all notice dates are NULL

**Severity: Critical — invalidates the entire post-notice analysis.**

The Retraction Watch CSV stores dates as `"M/D/YYYY 0:00"` (e.g. `"1/21/2026 0:00"`, `"10/15/2022 0:00"`). This is the format Crossref's export has used for every row in the current snapshot (verified on rows 1–4 and on the loaded DuckDB).

`util.parse_date` (`legacy/src/retraction_pollution/util.py:74-93`) handles:
- `%Y-%m-%d`, `%Y/%m/%d`, `%m/%d/%Y`, `%d/%m/%Y` — **but only when the string matches the format exactly.** `"1/21/2026 0:00"` does not match `%m/%d/%Y` because of the trailing ` 0:00`.
- Year-only `\d{4}` and year-month `\d{4}-\d{2}`.
- ISO `date.fromisoformat(text[:10])` — `"1/21/2026"` is not ISO, so this also fails.

Result: `parse_date("1/21/2026 0:00")` returns `None`.

Verified against the live 2.2 GB `opencitations.duckdb`:

| column | total resolved seeds | non-NULL |
|---|---|---|
| `notice_date` | 62,337 | **0** |
| `original_paper_date` | 62,337 | **0** |

Every downstream analysis that depends on the notice date is therefore structurally broken:

- `_seed_metrics` (`analysis.py:117-128`): the `if not pd.isna(notice_date)` guard is never entered, so `post_notice_direct_citers` and `post_notice_depth2_descendants` are always **0** for every seed.
- `_summary_metrics` (`analysis.py:203-207`): `post_notice_direct_citers` summary is always 0.
- `_plot_post_notice_timeline` (`analysis.py:282-312`): the `years` list is always empty, so the figure is never produced. The existing `outputs/figures/` contains only `depth_counts.*` — the post-notice timeline is missing, confirming this.
- The "post-notice" framing is the study's headline contribution; without it the pipeline produces only a static citation-network census.

**Fix:** extend `parse_date` to strip a trailing time component, e.g. `text = text.split()[0]` before the format loop, or add `%m/%d/%Y %H:%M` and `%-m/%-d/%Y %H:%M` formats. The R rewrite must handle this from the start.

### CRITICAL-2: Analysis was never run on the real OpenCitations-only data

The 2.2 GB `data/processed/opencitations.duckdb` is the only completed crawl (62337 resolved seeds, 2,655,541 works, 3,529,449 edges, complete to depth 2). But `outputs/opencitations/` does not exist — `rpollute analyze --db data/processed/opencitations.duckdb --output-dir outputs/opencitations` was never executed against the finished crawl.

The `outputs/` directory that does exist was produced from the *unresolved* OpenAlex DB (`study.duckdb`, 0 resolved seeds): `outputs/tables/summary.csv` shows `seed_records=68238, resolved_seed_records=0, depth2_nodes=0`. So the only analysis output present is a null run.

**Impact:** No results have actually been generated from the real data. Any conclusion drawn so far is from an empty graph. The R rewrite must run analysis end-to-end on the completed crawl before any claim can be made.

---

## 3. High-severity findings

### HIGH-1: `analysis.py` and `report.py` have zero test coverage

Confirmed by exhaustive test inventory (see §6). The 312-line analysis module — which computes every headline metric — is untested. Bugs like CRITICAL-1 survived because there is no test that round-trips a real RW row through `parse_date` → `row_to_seed` → `_seed_metrics` and asserts a non-zero post-notice count.

### HIGH-2: `add_edge` uses `INSERT OR IGNORE` — first-writer-wins on conflicting edge metadata

`storage.py:307-314`:

```python
INSERT OR IGNORE INTO citation_edges
    (source_id, target_id, depth, source_api, citation_date)
VALUES (?, ?, ?, ?, ?)
```

The primary key is `(source_id, target_id)`. If the *same* citing→cited pair is discovered at depth 1 via OpenCitations and later at depth 2 via OpenAlex (or vice versa), the **first** `depth` and `source_api` values win and the second insertion is silently dropped.

This is methodologically loaded because:
- `_seed_metrics` and `_bridge_metrics` filter edges by `depth` to classify direct citers vs depth-2 descendants. An edge mislabeled with the wrong depth silently misclassifies the citers.
- `edges["source_api"]` is summed in `_summary_metrics` to report the OpenCitations vs OpenAlex contribution split. A first-writer-wins policy makes that split dependent on crawl *ordering*, not on a principled choice.

The OpenCitations-only crawl is internally consistent (all edges `source_api='opencitations'`, depth monotonic by construction), but the hybrid pipeline in `crawler.py` is not, and no test exercises the conflict path (see §6).

**Fix:** either (a) make `depth` the *minimum* over all discoveries (so an edge always carries the shallowest depth at which it was observed — principled for a BFS frontier) and record `source_api` from the shallowest discovery, or (b) keep a per-discovery edge table and deduplicate at analysis time. The R rewrite should pick (a) explicitly and document it.

### HIGH-3: Frontier depth promotion is correct but untested, and interacts with HIGH-2

`storage.add_frontier_node` (`storage.py:284-296`) lowers a node's recorded depth when it is rediscovered closer to a seed (`elif depth < existing[0]`). This is the right BFS behavior. But because `add_edge` is first-writer-wins (HIGH-2), an edge recorded when a node was first seen at depth 2 keeps `depth=2` even if the node is later promoted to depth 1. The edge table and the frontier table can disagree about a node's depth.

`analysis.py:51-60` joins `frontier_nodes` to filter edges by the *frontier* depth, not the *edge* depth, so for the depth-filtering this is fine. But `_seed_metrics` (`analysis.py:111-115`) classifies a citer as "depth 2" by reading `node_depth.get(source_id) == 2` from the *nodes* frame (which comes from `frontier_nodes.depth`), so promotion is respected there too. The inconsistency only manifests in the `edges.depth` column itself and in the `source_api` split — both reported in `summary.csv`. Worth fixing in the rewrite.

### HIGH-4: `_bridge_metrics` is O(n·m) and uses a fragile empty-DataFrame constructor

`analysis.py:161-198`:

```python
for node_id in direct_nodes:
    cited_seed_count = len({target_id for target_id in edges.loc[edges["source_id"]==node_id, "target_id"] if ...})
    depth2_citer_count = len({source_id for source_id in edges.loc[edges["target_id"]==node_id, "source_id"] if ...})
```

For each depth-1 node (~773k in the real run) this scans the full 3.5M-edge frame twice. That is ~5.4 billion row scans — it will not finish in any reasonable time on the real data. The empty-return branch (`analysis.py:190-197`) uses `nodes.iloc[0:0].assign(cited_seed_count=[], depth2_citer_count=[])` which is a fragile pandas idiom that breaks if the dtypes don't line up; the non-empty branch falls back to `__import__("pandas")` inside the function, which is a code smell.

**Fix:** rewrite as two grouped joins (`edges.groupby('source_id')['target_id']` filtered to depth-0 targets; `edges.groupby('target_id')['source_id']` filtered to depth-2 sources). The R rewrite should use `dplyr` joins or `data.table` keyed lookups.

### HIGH-5: `_seed_metrics` is also O(seeds · edges) in the inner loop

`analysis.py:118-128` iterates each seed's direct citers and depth-2 descendants in Python, calling `pd.to_datetime` per source_id. With 62k seeds × hundreds of citers each, this is slow but not catastrophic; the bigger issue is that it depends entirely on `notice_date` being non-NULL, which CRITICAL-1 breaks.

### HIGH-6: No seed de-duplication by `OriginalPaperDOI`

Multiple Retraction Watch records can share the same `OriginalPaperDOI` (a paper retracted once and later given an EOC, or duplicate records). `upsert_seed` keys on `record_id` only (`storage.py:133-168`), so the same retracted paper becomes multiple seed nodes (multiple depth-0 frontier nodes), inflating seed counts and splitting the descendant network. The loaded DB has 62,337 resolved seeds from 68,249 records — the real unique-paper count is lower.

**Impact:** "seed_records" and "resolved_seed_records" in the summary overcount unique retracted papers. Any per-seed metric (`top_polluted_seeds.csv`) will list the same paper multiple times. The R rewrite should deduplicate seeds by `OriginalPaperDOI` (keeping the earliest/most-severe notice) before building the frontier.

---

## 4. Medium-severity findings

### MED-1: Title/author fallback resolution is opt-in and never run on the real data

`resolve_pending_seeds` with `title_fallback=False` (the default in `run-all` and `prepare-seeds`) marks every seed without DOI+PMID as `pending_title_fallback` and never resolves them (`crawler.py:27-43`). In the loaded DB, 5,912 seeds are `no_doi` and were simply dropped from the OpenCitations-only crawl. The OpenAlex hybrid run was never completed (0 resolved seeds in `study.duckdb`). So the seed universe is *DOI-bearing Retraction Watch records only* — a documented but unstudied selection bias against older, non-DOI literature.

### MED-2: OpenCitations-only crawl marks `is_retracted=False` for all citers

`opencitations_pipeline.py:159` hard-codes `is_retracted=False` for every citing work. This is fine for depth-1/2 citers in general, but when a retracted paper cites another retracted paper (24,919 such edges in the real data — see §5), the *citing* work is in fact retracted and the field is wrong. `analysis.py` does not currently use `is_retracted` on non-seed nodes, so this is latent, but the R rewrite should propagate retraction status from the seeds table into `works.is_retracted` for any work that is also a seed.

### MED-3: `notice_type` filter uses substring matching

`rw.is_seed_notice` (`rw.py:55-57`): `any(notice in text for notice in NOTICE_TYPES)` with `NOTICE_TYPES = ("retraction", "expression of concern")`. This matches `"Retraction"` and `"Expression of concern"` correctly, but also matches `"Retraction Watch"` if it ever appeared in `RetractionNature` (it does not in the current CSV, but the filter is not robust). More importantly, it includes `"Correction; Retraction"` combos (verified by `test_rw.py:6`) — which is defensible but should be documented as a deliberate inclusion.

### MED-4: `_plot_post_notice_timeline` uses `edge["target_id"]` to look up the seed notice date

`analysis.py:294`: `seed_notice.get(edge["target_id"])`. This assumes the edge's target is always a seed when computing post-notice citations. But `_plot_post_notice_timeline` iterates **all** edges, not just edges targeting depth-0 seeds. For a depth-2 edge (a depth-2 node citing a depth-1 node), `target_id` is the depth-1 node, which is not in `seed_notice`, so it is skipped — correct by accident. For a depth-1 edge targeting a seed, it works. The logic is right but only because of the `if notice_date is None: continue` guard; it would be clearer to filter to `edges.target_id.isin(seed_ids)` first.

### MED-5: Depth-3 cap is checked *per parent* in OpenCitations and *per page* in OpenAlex, with no shared budget

`crawler._depth_cap_reached` (`crawler.py:332-339`) checks `count_frontier_depth(3) >= depth3_node_cap` and `depth3_pages >= depth3_page_cap`. In the hybrid pipeline, OpenCitations supplement (`crawler.py:129-132`) checks the node cap per parent but not the page cap (it has no pages). OpenAlex (`crawler.py:285-290`) checks both. The two sources can collectively overshoot the node cap by up to one OpenAlex page (100 nodes) or one OpenCitations parent's entire citation list. Minor, but the report should disclose that depth-3 is *approximately* capped.

### MED-6: `report.md` is a static template, not a results report

`report.py` writes a fixed boilerplate + a dump of `summary.csv` + the raw `last_crawl_summary` JSON blob. It does not narrate findings, state the sample size, list the top polluted seeds, or include the figures. For a study report this is inadequate; it is closer to a run log. The R rewrite should produce a real report (knitted R Markdown / Quarto) with prose, tables, and embedded figures.

### MED-7: No provenance / no snapshot date in outputs

The pipeline downloads Retraction Watch into a timestamped file but never writes the snapshot date into `report.md`, the summary table, or run metadata. A reader of `outputs/tables/summary.csv` cannot tell which RW snapshot it was derived from. OpenCitations and OpenAlex snapshots are not recorded at all. For reproducibility the rewrite must record source snapshots and access dates.

### MED-8: No statistical analysis

The pipeline computes counts and reach metrics but no uncertainty, no confidence intervals, no comparison group (e.g. post-notice citation rate vs pre-notice baseline, or vs a matched non-retracted control). The "pollution" framing implies a counterfactual; the code does not estimate one. This is a methodological gap the rewrite could address if the goal is a publishable analysis.

---

## 5. Edge-direction and data-integrity sanity checks (performed against the live 2.2 GB DB)

These checks confirm the parts of the pipeline that *are* sound.

| check | result | verdict |
|---|---|---|
| Edge direction `source cites target` | depth-1 edges have seed as `target_id`, citers as `source_id` | ✅ correct |
| Seeds appearing as `source` of depth-1 edges | 23,221 — all are retracted-citing-retracted (target is also a seed, 24,919 such edges when including depth-2) | ✅ legitimate, not a direction bug |
| Duplicate `(source,target)` pairs | 3,529,449 total = 3,529,449 distinct | ✅ no dup edges in OC-only run |
| `publication_date` coverage on depth-1/2 nodes | depth-1: 769,016 / 773,146 (99.2%); depth-2: 1,812,849 / 1,820,090 (99.6%) | ✅ good — date-based analysis is feasible *except* for the broken notice date |
| Seed count | 68,249 loaded, 62,337 DOI-resolved, 5,912 no_doi | ⚠️ see HIGH-6 (no DOI dedup) |
| Depth distribution | depth-0: 60,305; depth-1: 773,146; depth-2: 1,820,090 | ✅ consistent with BFS |
| `pipeline_mode` metadata | `opencitations` | ✅ |
| Depth-3 truncation | not reached (depth-3 frontier is empty — crawl stopped at `complete_depth=2`) | ✅ |

The OpenCitations-only crawler's core graph construction is sound. The defects are concentrated in (a) date parsing, (b) the analysis layer, and (c) test coverage.

---

## 6. Test coverage audit

**Suite status:** 18 tests, all pass (`uv run pytest -q` → `18 passed in 2.73s`) once `duckdb` is installed via `uv sync --all-extras`. Without dev extras, 6 tests fail with `RuntimeError: DuckDB is required` — the `pyproject.toml` `dev` extra includes `pytest` but the core `duckdb` dependency is in the main group, so this is a non-issue once `uv sync` is run. Still, the test runner should not require `--all-extras` to install the project's own runtime dependency.

### Modules with NO test file

| module | LOC | risk |
|---|---|---|
| `analysis.py` | 312 | **High** — computes every headline metric; CRITICAL-1 hid here |
| `report.py` | 71 | Low — boilerplate |
| `cli.py` | 355 | Medium — arg wiring, `opencitations_only_settings` path logic |
| `config.py` | 66 | Low — dataclass + env |
| `__init__.py` | 3 | None |

### What the existing tests *do* cover well

- `test_util.py` (4 tests): `clean_doi`, `clean_pmid`, `parse_date` (partial-date formats only — **not** the `M/D/YYYY 0:00` format that actually occurs in RW data, which is why CRITICAL-1 was missed), `compact_openalex_id`, `first_author_last_name` NaN safety. Good edge-case habits, wrong format coverage.
- `test_rw.py` (2 tests): `is_seed_notice` filter combos, `load_seed_rows` end-to-end on a synthetic CSV — but the synthetic CSV uses `'2023-03'` style dates, not the real `'M/D/YYYY 0:00'` format. **This is the gap that let CRITICAL-1 through.**
- `test_storage.py` (2 tests): two regression tests (DuckDB binder bug, None-DOI round-trip). Narrow.
- `test_openalex.py` (4 tests): `normalize_work`, `edges_from_work`, DOI-filter sanitization, `get_works_by_dois` bisection-on-400. Strong, behavior-asserting.
- `test_opencitations.py` (2 tests): `extract_pid`, `parse_open_citation`. Good.
- `test_opencitations_pipeline.py` (1 test): full depth-2 crawl with a fake client, asserts exact edge tuples and depths. Best-asserted test in the suite, but only one case.
- `test_crawler.py` (3 tests): `resolve_pending_seeds` no-network paths only. The `CitationCrawler` class itself is untested.

### Specific untested behaviors (confirmed)

- `add_frontier_node` depth promotion (`storage.py:293-296`) — **not tested**.
- `add_edge` `INSERT OR IGNORE` conflict behavior (HIGH-2) — **not tested**.
- `CitationCrawler.crawl` / `_crawl_level` / cursor resume (`crawler.py:206-316`) — **not tested**.
- `supplement_level_from_opencitations` depth-3 truncation (`crawler.py:129-132`) — **not tested**.
- `_store_opencitations_for_parent` PMID fallback path (`crawler.py:181-204`) — **not tested**.
- `_store_citations` dual-edge branch when `cited_doi != parent_doi` (`opencitations_pipeline.py:178-185`) — **not tested** (fake client always sets `cited == parent`).
- `OpenCitationsClient._request_json` retry/404/429 paths — **not tested**.
- `OpenAlexClient._request_json` retry/429/5xx paths, `_retry_after_seconds`, `rate_limit_status` — **not tested**.
- `run_analysis`, `_seed_metrics`, `_bridge_metrics`, `_summary_metrics`, all `_plot_*` — **not tested**.
- `write_report` — **not tested**.

### Test quality

The 18 existing tests assert behavior (not just smoke), use proper fakes for network (e.g. `FakeOpenAlexClient` whose methods `raise AssertionError` to prove no-network paths), and avoid flakiness. The problem is *coverage*, not *quality*. The analysis layer — the part that produces the science — has zero tests, and that is where the critical bug lives.

---

## 7. Lower-severity / code-quality notes

- `analysis.py:195` uses `__import__("pandas")` inside a function that already receives `pd` as a parameter — dead code smell.
- `analysis.py:248` `value != value` NaN check is correct but fragile; `pd.isna` is clearer.
- `storage.unresolved_seeds` (`storage.py:208-210`) builds SQL with f-string `LIMIT {int(limit)}` — safe because of the `int()` cast, but parameterized would be idiomatic.
- `openalex._request_json` sleeps on attempt 0 unconditionally (`if attempt or self.request_delay`) — fine, but the `2.0**attempt` backoff on retries is capped at 60s, while `_retry_after_seconds` caps at 300s; the two cap values are inconsistent.
- `opencitations.py:60` does `import json` inside the function on every request — micro-inefficiency.
- `report.py:23` `datetime.now(UTC).isoformat()` is fine; the report itself is the weak point (MED-6).
- The `.env.example` exists but `config.py` reads only `os.getenv` — no `.env` loading. README says `export` which is correct, but the `.env.example` file is misleading.
- `crawl_jobs` table is created but `crawl_jobs` count is 0 in the OC-only DB — the OC-only crawler never creates jobs (it has no cursor pagination). The table is dead weight in that mode.

---

## 8. Reproducibility assessment

| dimension | status |
|---|---|
| Source data snapshot | RW CSV is downloaded with a timestamp filename, but the timestamp is not propagated into outputs or metadata. ❌ |
| API snapshot date | Not recorded for OpenCitations or OpenAlex. ❌ |
| Software version | `pyproject.toml` pins lower bounds only (`>=`), `uv.lock` exists and pins exact versions. ✅ (via lock file) |
| Random seeds | No randomness in the pipeline. ✅ |
| Crawl resumability | Cursor-based for OpenAlex, `processed_at`-based for OpenCitations. ✅ but untested. |
| Run metadata | `last_crawl_summary` is stored as JSON in `run_metadata`. ✅ but not surfaced in the report. |
| Final results | **Do not exist** for the completed OC-only crawl (CRITICAL-2). ❌ |

---

## 9. Summary of required fixes (priority order)

1. **Fix `parse_date` to handle `"M/D/YYYY 0:00"`** — strip time component before format matching. (CRITICAL-1)
2. **Run `analyze` + `report` on the completed OpenCitations-only crawl** and inspect actual results. (CRITICAL-2)
3. **Add tests for `analysis.py`** covering `_seed_metrics` post-notice counting with real RW date strings. (HIGH-1)
4. **Decide and document edge `depth`/`source_api` semantics on conflict** — replace `INSERT OR IGNORE` with a min-depth merge. (HIGH-2, HIGH-3)
5. **Rewrite `_bridge_metrics` and `_seed_metrics` as vectorized joins** — the current Python loops will not finish on 3.5M edges. (HIGH-4, HIGH-5)
6. **Deduplicate seeds by `OriginalPaperDOI`** before frontier construction. (HIGH-6)
7. **Propagate retraction status** from seeds into `works.is_retracted` for any work that is also a seed. (MED-2)
8. **Record source snapshot dates** in run metadata and in the report. (MED-7)
9. **Produce a real narrative report** (Quarto/R Markdown) with sample size, top seeds, figures, and provenance. (MED-6)
10. **Add a comparison group or baseline** if the goal is a publishable "pollution" claim. (MED-8)

---

## 10. Bottom line

The data-acquisition layer (Retraction Watch parsing, OpenCitations DOI crawl, frontier/edge storage, BFS depth control) is **structurally sound** and has produced a real, internally consistent 2.65M-node / 3.53M-edge graph.

The analysis layer is **not sound**: a single date-parsing bug nullifies every notice-date-dependent metric (the study's headline), the analysis was never actually run on the completed graph, two of the core metric functions will not scale to the real data, edge-conflict semantics are unspecified, and the analysis module has no tests.

The study is **not 100% sound** as it stands. The R rewrite should fix CRITICAL-1 and CRITICAL-2 first, then address HIGH-2 through HIGH-6 before producing any result, and should ship with tests that round-trip real Retraction Watch rows through the full parse → seed → analyze path.