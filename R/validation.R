#' @title Study artifact validation
#' @description Read-only checks that reconcile a DuckDB study store with the
#'   generated CSV/report artifacts.

#' @noRd
validation_row <- function(check, severity, status, observed = "",
                           expected = "", details = "") {
  data.frame(
    check = check,
    severity = severity,
    status = status,
    observed = as.character(observed),
    expected = as.character(expected),
    details = as.character(details),
    stringsAsFactors = FALSE
  )
}

validation_status <- function(ok) {
  if (isTRUE(ok)) "pass" else "fail"
}

metadata_keys <- function(store) {
  df <- DBI::dbGetQuery(store$con, "SELECT key FROM run_metadata")
  as.character(df$key)
}

db_single_row <- function(store, sql) {
  DBI::dbGetQuery(store$con, sql)[1, , drop = FALSE]
}

validation_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) NULL
  )
}

#' Validate generated study artifacts against the backing store.
#'
#' @param store StudyStore.
#' @param output_dir output root containing `tables/`, `figures/`, and reports.
#' @param max_analysis_depth integer depth included in analysis exports.
#' @param include_report logical, whether report existence should be checked.
#' @return data.frame of validation checks; also writes
#'   `tables/artifact_validation.csv` when possible.
#' @export
validate_study_outputs <- function(store, output_dir, max_analysis_depth = 2L,
                                   include_report = TRUE) {
  table_dir <- file.path(output_dir, "tables")
  ensure_dir(table_dir)
  rows <- list()

  report_exists <- file.exists(file.path(output_dir, "report.md")) ||
    file.exists(file.path(output_dir, "report.html"))
  rows[[length(rows) + 1L]] <- validation_row(
    "report_exists", "major",
    if (include_report) validation_status(report_exists) else "not_checked",
    report_exists, TRUE,
    "Narrative report should exist next to generated tables and figures."
  )

  keys <- metadata_keys(store)
  required <- c("rw_snapshot_date", "oc_access_date", "pipeline_mode",
                "opencitations_seed_stats", "last_crawl_summary",
                "depth3_truncated")
  missing_keys <- setdiff(required, keys)
  rows[[length(rows) + 1L]] <- validation_row(
    "required_metadata", "major", validation_status(length(missing_keys) == 0L),
    paste(sort(intersect(required, keys)), collapse = ";"),
    paste(required, collapse = ";"),
    if (length(missing_keys) == 0L) "" else
      paste("Missing:", paste(missing_keys, collapse = ";"))
  )

  duplicate_sql <- "
    WITH linked AS (
      SELECT openalex_id, resolved_status
      FROM seeds
      WHERE openalex_id IS NOT NULL
    ),
    canonical AS (
      SELECT openalex_id
      FROM linked
      WHERE resolved_status = 'resolved'
    ),
    duplicate_groups AS (
      SELECT openalex_id, COUNT(*) AS n
      FROM canonical
      GROUP BY openalex_id
      HAVING COUNT(*) > 1
    ),
    linked_duplicate_groups AS (
      SELECT openalex_id, COUNT(*) AS n
      FROM linked
      GROUP BY openalex_id
      HAVING COUNT(*) > 1
    )
    SELECT
      (SELECT COUNT(*) FROM canonical) AS resolved_seed_records,
      (SELECT COUNT(DISTINCT openalex_id) FROM linked) AS resolved_seed_papers,
      COALESCE((SELECT SUM(n) FROM duplicate_groups), 0) AS duplicate_rows,
      COALESCE((SELECT SUM(n) FROM linked_duplicate_groups), 0) AS linked_duplicate_rows
  "
  dup <- db_single_row(store, duplicate_sql)
  rows[[length(rows) + 1L]] <- validation_row(
    "duplicate_canonical_seed_records", "major",
    validation_status(as.integer(dup$duplicate_rows[1]) == 0L),
    as.integer(dup$duplicate_rows[1]), 0L,
    paste0("Resolved records=", dup$resolved_seed_records[1],
           "; resolved papers=", dup$resolved_seed_papers[1],
           "; linked duplicate rows=", dup$linked_duplicate_rows[1])
  )

  edge_sql <- "
    WITH frontier AS (
      SELECT openalex_id
      FROM frontier_nodes
      WHERE depth <= {depth}
    ),
    classified AS (
      SELECT
        fs.openalex_id AS source_node,
        ft.openalex_id AS target_node
      FROM citation_edges e
      LEFT JOIN frontier fs ON e.source_id = fs.openalex_id
      LEFT JOIN frontier ft ON e.target_id = ft.openalex_id
    )
    SELECT
      COUNT(*) AS stored_edges,
      SUM(CASE WHEN source_node IS NULL THEN 1 ELSE 0 END) AS missing_sources,
      SUM(CASE WHEN target_node IS NULL THEN 1 ELSE 0 END) AS missing_targets,
      SUM(CASE WHEN source_node IS NOT NULL AND target_node IS NOT NULL
               THEN 1 ELSE 0 END) AS exported_like
    FROM classified
  "
  edge_sql <- sub("\\{depth\\}", as.integer(max_analysis_depth), edge_sql)
  edge_counts <- db_single_row(store, edge_sql)
  rows[[length(rows) + 1L]] <- validation_row(
    "stored_vs_exported_edges", "major",
    validation_status(as.integer(edge_counts$stored_edges[1]) ==
                        as.integer(edge_counts$exported_like[1])),
    paste0("stored=", edge_counts$stored_edges[1],
           "; exported_like=", edge_counts$exported_like[1],
           "; missing_targets=", edge_counts$missing_targets[1]),
    "stored edges equal exported induced frontier edges",
    "If this fails, summary/report must disclose the induced-edge filter."
  )

  graph_path <- file.path(output_dir, "graphs", "network_depth2.graphml.gz")
  graph_manifest_path <- file.path(output_dir, "graphs",
                                   "network_depth2.graphml.json")
  graph_exists <- file.exists(graph_path)
  graph_manifest_exists <- file.exists(graph_manifest_path)
  if (!graph_exists && !graph_manifest_exists) {
    rows[[length(rows) + 1L]] <- validation_row(
      "full_graph_export", "major", "not_checked",
      "graph export absent", "GraphML gzip plus manifest",
      "Full graph export is optional for small or intermediate runs."
    )
  } else {
    graph_ok <- FALSE
    observed <- paste0("graph_exists=", graph_exists,
                       "; manifest_exists=", graph_manifest_exists)
    if (graph_exists && graph_manifest_exists) {
      manifest <- tryCatch(
        jsonlite::read_json(graph_manifest_path),
        error = function(e) NULL
      )
      if (!is.null(manifest)) {
        graph_ok <- as.integer(manifest$node_count) ==
          as.integer(DBI::dbGetQuery(
            store$con,
            paste0("SELECT COUNT(*) AS n FROM frontier_nodes WHERE depth <= ",
                   as.integer(max_analysis_depth))
          )$n[1]) &&
          as.integer(manifest$edge_count) ==
          as.integer(edge_counts$exported_like[1])
        observed <- paste0(
          "manifest_nodes=", manifest$node_count,
          "; manifest_edges=", manifest$edge_count,
          "; graph_exists=", graph_exists
        )
      }
    }
    rows[[length(rows) + 1L]] <- validation_row(
      "full_graph_export", "major", validation_status(graph_ok),
      observed, "manifest counts match database graph counts",
      "Full network graph should be exported as compressed GraphML for complete large studies."
    )
  }

  frontier_sql <- "
    SELECT
      depth,
      COUNT(*) AS node_count,
      SUM(CASE WHEN processed_at IS NULL THEN 1 ELSE 0 END) AS pending_count
    FROM frontier_nodes
    WHERE depth < {depth}
    GROUP BY depth
    ORDER BY depth
  "
  frontier_sql <- sub("\\{depth\\}", as.integer(max_analysis_depth),
                      frontier_sql)
  frontier <- DBI::dbGetQuery(store$con, frontier_sql)
  pending <- if (nrow(frontier) == 0L) 0L else sum(frontier$pending_count)
  rows[[length(rows) + 1L]] <- validation_row(
    "frontier_processed_for_depth2_claim", "major",
    validation_status(as.integer(pending) == 0L),
    as.integer(pending), 0L,
    "All depth 0 and depth 1 parents must be processed before claiming a complete depth-2 graph."
  )

  failed_parents <- db_single_row(
    store,
    "SELECT COUNT(*) AS n FROM run_metadata
     WHERE key LIKE 'opencitations_failed_parent:%'"
  )
  rows[[length(rows) + 1L]] <- validation_row(
    "opencitations_failed_parents", "moderate",
    validation_status(as.integer(failed_parents$n[1]) == 0L),
    as.integer(failed_parents$n[1]), 0L,
    "Failed parents should be disclosed in report limitations."
  )

  topic_path <- file.path(table_dir, "topic_distribution.csv")
  topics <- validation_read_csv(topic_path)
  topic_has_labels <- !is.null(topics) &&
    any(!is.na(topics$topic_name) | !is.na(topics$topic_domain))
  rows[[length(rows) + 1L]] <- validation_row(
    "topic_labels_present", "moderate", validation_status(topic_has_labels),
    topic_has_labels, TRUE,
    "OpenCitations-only outputs need metadata enrichment before topic claims."
  )

  bridge_path <- file.path(table_dir, "bridge_papers.csv")
  bridges <- validation_read_csv(bridge_path)
  bridge_titles <- !is.null(bridges) && "title" %in% names(bridges) &&
    any(!is.na(bridges$title))
  rows[[length(rows) + 1L]] <- validation_row(
    "bridge_titles_present", "moderate", validation_status(bridge_titles),
    bridge_titles, TRUE,
    "Bridge-paper interpretation requires non-missing work titles."
  )

  validation <- do.call(rbind, rows)
  readr::write_csv(validation, file.path(table_dir, "artifact_validation.csv"))
  validation
}
