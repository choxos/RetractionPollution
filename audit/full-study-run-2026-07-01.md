# Full Study Run Evidence - 2026-07-01

## Scope

Completed the OpenCitations depth-2 study in `legacy/data/processed/opencitations.duckdb` and regenerated `outputs/opencitations`.

Depth 2 is the terminal frontier. A complete depth-2 study requires all depth 0 and depth 1 parents to be processed; depth 2 nodes are retained as terminal descendants and are not expanded to depth 3.

## Source Inputs

- OpenCitations DOI citation-count dump: `10.6084/m9.figshare.24199089.v6`
  - Local file: `data/external/opencitations_doi_citation_count.zip`
  - MD5: `74baf39497eb27387d82c3e2a11b9aac`
  - Snapshot described by source: February 2026 OpenCitations Index
- OpenCitations Index CSV dataset: `10.6084/m9.figshare.24356626.v7`
  - Local manifest: `data/external/opencitations_index_files.csv`
  - Files processed: 165
  - Total source ZIP bytes represented in manifest: 34,645,070,871
- OpenCitations Meta OMID mapping:
  - Zenodo record: `https://zenodo.org/records/18980639`
  - Local file: `data/external/ocmeta_omid.zip`
  - MD5: `bcab712c96b3254d62be5f4c6b058d01`
  - Snapshot described by source: January 2026 OpenCitations Meta

## Completion Evidence

Final frontier state:

| depth | node_count | processed_count | pending_count |
|---:|---:|---:|---:|
| 0 | 60,305 | 60,305 | 0 |
| 1 | 791,269 | 791,269 | 0 |
| 2 | 10,659,409 | 0 | 10,659,409 |

Final stored citation edges after stale out-of-frontier cleanup: 31,563,370.

Failed parent metadata rows after completion: 0.

## Bulk And REST Reconciliation

- Pending nonzero DOI depth-1 parents mapped to OMIDs: 585,530.
- Bulk extracted citation rows: 28,010,255.
- Unique citing OMIDs: 10,717,180.
- Unique citing OMIDs mapped to DOI or PMID: 10,716,055.
- Citation-count reconciliation:
  - Exact parents: 575,358.
  - Undercount parents: 985.
  - Overcount parents: 9,187.
- Targeted REST fill for all undercount parents:
  - Parents processed: 985 of 985.
  - Failures: 0.
  - Citation rows returned: 36,558.
- Stale edge cleanup:
  - Removed 4,737 old `citation_edges` rows whose target IDs were outside the completed frontier.
  - Unique missing targets removed: 77.

## Regenerated Outputs

- `outputs/opencitations/report.html`
- `outputs/opencitations/report.md`
- `outputs/opencitations/tables/nodes_depth2.csv` (946.7 MB)
- `outputs/opencitations/tables/edges_depth2.csv` (2.589 GB)
- `outputs/opencitations/tables/top_polluted_seeds.csv`
- `outputs/opencitations/tables/bridge_papers.csv`
- `outputs/opencitations/tables/depth_counts.csv`
- `outputs/opencitations/tables/summary.csv`
- `outputs/opencitations/tables/artifact_validation.csv`
- `outputs/opencitations/manuscript.md`
- `outputs/opencitations/manuscript.html` (self-contained embedded HTML)
- `outputs/opencitations/manuscript.pdf`
- `outputs/opencitations/manuscript.docx`
- `outputs/opencitations/figures/manuscript_figure2_full_network_density.png`
- `outputs/opencitations/graphs/network_depth2.graphml.gz`
- `outputs/opencitations/graphs/network_depth2.graphml.json`
- `outputs/opencitations/tables/network_density_node_bins.csv`
- `outputs/opencitations/tables/network_density_edge_bins.csv`

The full-size graph is exported as compressed GraphML at `outputs/opencitations/graphs/network_depth2.graphml.gz`. Its manifest reports 11,510,983 nodes and 31,563,370 edges. The stale prior uncompressed GraphML was renamed and is not the current graph artifact.

The manuscript includes a full-network density graph generated from all 31,563,370 edges. Nodes are ranked by citation degree within depth and aggregated into 256 bins per layer; every edge contributes to the density ribbons between bins. The rendered manuscript outputs were regenerated as embedded HTML, PDF, and DOCX so Figure 2 is visible inside the manuscript rather than depending on a sidecar image path.

## Validation

Major completeness checks passed:

- `report_exists`
- `required_metadata`
- `duplicate_canonical_seed_records`
- `stored_vs_exported_edges`
- `frontier_processed_for_depth2_claim`
- `opencitations_failed_parents`

Remaining moderate validation failures are metadata enrichment limitations of the OpenCitations-only output:

- `topic_labels_present`: fail, because OpenAlex topic enrichment was not run.
- `bridge_titles_present`: fail, because DOI/PMID OpenCitations nodes do not carry work titles without metadata enrichment.

## Package Verification

- `testthat`: passed.
  - One CRAN-only skip.
  - Warnings: locally installed `duckdb` and `igraph` were built under R 4.6.1.
- `R CMD build`: passed from a staged package source that excluded generated study artifacts up front.
- `R CMD check --no-manual --no-build-vignettes`: exit code 0.
  - Warning: locally installed `duckdb` and `igraph` were built under R 4.6.1.
  - Warning: broad undocumented exported objects due current package export/documentation policy.
