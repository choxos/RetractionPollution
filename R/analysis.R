#' @title Analysis module for retractionpollution
#' @description Vectorized port of the Python `retraction_pollution.analysis`
#'   module. The Python version carried two critical/high-severity bugs that
#'   this port fixes:
#'   * CRITICAL-1: date parsing silently dropped notice dates, so every
#'     post-notice metric returned 0. Here we rely on the fixed `parse_date`
#'     and treat `notice_date` as a real `Date`.
#'   * HIGH-4/HIGH-5: per-row Python loops over edges (`O(n*m)`) are replaced
#'     with dplyr joins so the whole pipeline runs in vectorized form.

library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(igraph)

#' Run the full analysis pipeline on a study store.
#'
#' @param store `StudyStore` instance.
#' @param output_dir character path to the analysis output root.
#' @param max_analysis_depth integer maximum frontier depth to include.
#' @return named list of summary metrics.
#' @export
run_analysis <- function(store, output_dir, max_analysis_depth = 2L) {
  table_dir <- ensure_dir(file.path(output_dir, "tables"))
  figure_dir <- ensure_dir(file.path(output_dir, "figures"))
  graph_dir <- ensure_dir(file.path(output_dir, "graphs"))

  scale <- analysis_scale_counts(store, max_analysis_depth)
  large_edge_limit <- getOption(
    "retractionpollution.analysis_in_memory_edge_limit",
    5000000L
  )
  if (is.finite(large_edge_limit) &&
      scale$exported_edges > large_edge_limit) {
    return(run_analysis_duckdb(
      store = store,
      output_dir = output_dir,
      max_analysis_depth = max_analysis_depth,
      scale_counts = scale
    ))
  }

  nodes_sql <- "
    SELECT f.openalex_id, f.depth, w.doi, w.title, w.publication_date,
           w.publication_year, w.work_type, w.is_retracted, w.cited_by_count,
           w.source_name, w.topic_name, w.topic_domain
    FROM frontier_nodes f
    LEFT JOIN works w ON f.openalex_id = w.openalex_id
    WHERE f.depth <= ?
  "
  nodes <- tibble::as_tibble(
    DBI::dbGetQuery(store$con, nodes_sql, params = list(as.integer(max_analysis_depth)))
  )

  edges_sql <- "
    SELECT e.source_id, e.target_id, e.depth, e.source_api, e.citation_date
    FROM citation_edges e
    JOIN frontier_nodes fs ON e.source_id = fs.openalex_id
    JOIN frontier_nodes ft ON e.target_id = ft.openalex_id
    WHERE fs.depth <= ? AND ft.depth <= ?
  "
  edges <- tibble::as_tibble(
    DBI::dbGetQuery(
      store$con, edges_sql,
      params = list(as.integer(max_analysis_depth), as.integer(max_analysis_depth))
    )
  )

  seeds <- tibble::as_tibble(DBI::dbGetQuery(store$con, "SELECT * FROM seeds"))

  readr::write_csv(nodes, file.path(table_dir, "nodes_depth2.csv"))
  readr::write_csv(edges, file.path(table_dir, "edges_depth2.csv"))

  depth_counts <- nodes |>
    dplyr::group_by(depth) |>
    dplyr::summarise(node_count = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(depth)
  readr::write_csv(depth_counts, file.path(table_dir, "depth_counts.csv"))

  top_seeds <- seed_metrics(seeds, nodes, edges)
  readr::write_csv(top_seeds, file.path(table_dir, "top_polluted_seeds.csv"))

  bridges <- bridge_metrics(nodes, edges)
  readr::write_csv(bridges, file.path(table_dir, "bridge_papers.csv"))

  topics <- nodes |>
    dplyr::filter(depth %in% c(1L, 2L)) |>
    dplyr::group_by(depth, topic_domain, topic_name) |>
    dplyr::summarise(node_count = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(depth, dplyr::desc(node_count))
  readr::write_csv(topics, file.path(table_dir, "topic_distribution.csv"))

  graph <- build_graph(nodes, edges)
  igraph::write_graph(graph, file.path(graph_dir, "network_depth2.graphml"),
                      format = "graphml")

  edge_scope <- analysis_edge_scope(store, max_analysis_depth)
  summary <- summary_metrics(graph, seeds, nodes, edges, top_seeds, edge_scope)
  summary_df <- tibble::tibble(
    metric = names(summary),
    value = as.character(unlist(summary, use.names = FALSE))
  )
  readr::write_csv(summary_df, file.path(table_dir, "summary.csv"))

  plot_depth_counts(depth_counts, figure_dir)
  plot_top_seeds(top_seeds, figure_dir)
  plot_post_notice_timeline(seeds, nodes, edges, figure_dir)

  summary
}

#' Count the analysis export size before choosing an execution path.
#' @param store StudyStore.
#' @param max_analysis_depth integer.
#' @return named list with node and edge counts.
analysis_scale_counts <- function(store, max_analysis_depth = 2L) {
  depth <- as.integer(max_analysis_depth)
  node_sql <- sprintf("
    SELECT COUNT(*) AS n
    FROM frontier_nodes
    WHERE depth <= %d
  ", depth)
  edge_sql <- sprintf("
    WITH frontier AS (
      SELECT openalex_id
      FROM frontier_nodes
      WHERE depth <= %d
    )
    SELECT
      COUNT(*) AS stored_edges,
      SUM(CASE WHEN fs.openalex_id IS NOT NULL
                 AND ft.openalex_id IS NOT NULL THEN 1 ELSE 0 END) AS exported_edges
    FROM citation_edges e
    LEFT JOIN frontier fs ON e.source_id = fs.openalex_id
    LEFT JOIN frontier ft ON e.target_id = ft.openalex_id
  ", depth)
  edge_counts <- DBI::dbGetQuery(store$con, edge_sql)
  list(
    nodes = as.numeric(DBI::dbGetQuery(store$con, node_sql)$n[1]),
    stored_edges = as.numeric(edge_counts$stored_edges[1]),
    exported_edges = as.numeric(edge_counts$exported_edges[1])
  )
}

#' Run a DuckDB-backed analysis path for large completed studies.
#'
#' The regular `run_analysis()` path materializes all nodes and edges in R and
#' builds an igraph object. That is appropriate for fixtures and smaller runs,
#' but not for multi-million-node OpenCitations studies. This path keeps the
#' large exports and aggregations inside DuckDB and only brings small summary
#' tables back into R for plotting and reporting.
#'
#' @param store StudyStore.
#' @param output_dir character.
#' @param max_analysis_depth integer.
#' @param scale_counts optional precomputed counts from `analysis_scale_counts`.
#' @return named list of summary metrics.
run_analysis_duckdb <- function(store, output_dir, max_analysis_depth = 2L,
                                scale_counts = NULL) {
  table_dir <- ensure_dir(file.path(output_dir, "tables"))
  figure_dir <- ensure_dir(file.path(output_dir, "figures"))
  graph_dir <- ensure_dir(file.path(output_dir, "graphs"))
  depth <- as.integer(max_analysis_depth)
  if (is.null(scale_counts)) {
    scale_counts <- analysis_scale_counts(store, depth)
  }

  nodes_sql <- sprintf("
    SELECT f.openalex_id, f.depth, w.doi, w.title, w.publication_date,
           w.publication_year, w.work_type, w.is_retracted, w.cited_by_count,
           w.source_name, w.topic_name, w.topic_domain
    FROM frontier_nodes f
    LEFT JOIN works w ON f.openalex_id = w.openalex_id
    WHERE f.depth <= %d
  ", depth)
  edges_sql <- sprintf("
    SELECT e.source_id, e.target_id, e.depth, e.source_api, e.citation_date
    FROM citation_edges e
    JOIN frontier_nodes fs ON e.source_id = fs.openalex_id
    JOIN frontier_nodes ft ON e.target_id = ft.openalex_id
    WHERE fs.depth <= %d AND ft.depth <= %d
  ", depth, depth)

  duckdb_copy_query(store$con, nodes_sql,
                    file.path(table_dir, "nodes_depth2.csv"))
  duckdb_copy_query(store$con, edges_sql,
                    file.path(table_dir, "edges_depth2.csv"))

  depth_counts <- DBI::dbGetQuery(store$con, sprintf("
    SELECT depth, COUNT(*) AS node_count
    FROM frontier_nodes
    WHERE depth <= %d
    GROUP BY depth
    ORDER BY depth
  ", depth))
  readr::write_csv(depth_counts, file.path(table_dir, "depth_counts.csv"))

  seeds <- tibble::as_tibble(DBI::dbGetQuery(store$con, "SELECT * FROM seeds"))
  canonical <- canonical_seed_records(seeds)
  canonical_table <- paste0("tmp_canonical_seeds_", Sys.getpid())
  DBI::dbWriteTable(store$con, canonical_table, canonical,
                    temporary = TRUE, overwrite = TRUE)
  canonical_ref <- DBI::dbQuoteIdentifier(store$con, canonical_table)

  create_large_analysis_temps(store$con, canonical_ref, depth)
  top_seeds <- large_seed_metrics(store$con, canonical_ref)
  readr::write_csv(top_seeds, file.path(table_dir, "top_polluted_seeds.csv"))

  bridge_sql <- sprintf("
    WITH depth1_nodes AS (
      SELECT f.openalex_id, w.title
      FROM frontier_nodes f
      LEFT JOIN works w ON f.openalex_id = w.openalex_id
      WHERE f.depth = 1
    ),
    cited_seed AS (
      SELECT e.source_id AS openalex_id,
             COUNT(DISTINCT e.target_id) AS cited_seed_count
      FROM citation_edges e
      JOIN frontier_nodes ft ON e.target_id = ft.openalex_id AND ft.depth = 0
      WHERE e.depth = 1
      GROUP BY e.source_id
    ),
    depth2_citer AS (
      SELECT e.target_id AS openalex_id,
             COUNT(DISTINCT e.source_id) AS depth2_citer_count
      FROM citation_edges e
      JOIN frontier_nodes fs ON e.source_id = fs.openalex_id AND fs.depth = 2
      WHERE e.depth = 2
      GROUP BY e.target_id
    )
    SELECT n.openalex_id, n.title,
           COALESCE(c.cited_seed_count, 0) AS cited_seed_count,
           COALESCE(d.depth2_citer_count, 0) AS depth2_citer_count
    FROM depth1_nodes n
    LEFT JOIN cited_seed c ON n.openalex_id = c.openalex_id
    LEFT JOIN depth2_citer d ON n.openalex_id = d.openalex_id
    WHERE COALESCE(c.cited_seed_count, 0) > 0
       OR COALESCE(d.depth2_citer_count, 0) > 0
    ORDER BY depth2_citer_count DESC, cited_seed_count DESC
  ")
  duckdb_copy_query(store$con, bridge_sql,
                    file.path(table_dir, "bridge_papers.csv"))

  topics <- DBI::dbGetQuery(store$con, sprintf("
    SELECT f.depth, w.topic_domain, w.topic_name, COUNT(*) AS node_count
    FROM frontier_nodes f
    LEFT JOIN works w ON f.openalex_id = w.openalex_id
    WHERE f.depth IN (1, 2) AND f.depth <= %d
    GROUP BY f.depth, w.topic_domain, w.topic_name
    ORDER BY f.depth, node_count DESC
  ", depth))
  readr::write_csv(topics, file.path(table_dir, "topic_distribution.csv"))

  graph_export <- export_full_network_graph_graphml_gz(
    con = store$con,
    graph_dir = graph_dir,
    max_analysis_depth = depth,
    scale_counts = scale_counts
  )

  edge_scope <- analysis_edge_scope(store, depth)
  source_counts <- DBI::dbGetQuery(store$con, sprintf("
    WITH exported_edges AS (%s)
    SELECT source_api, COUNT(*) AS n
    FROM exported_edges
    GROUP BY source_api
  ", edges_sql))
  opencitations_edges <- sum(
    source_counts$n[source_counts$source_api == "opencitations"],
    na.rm = TRUE
  )
  openalex_edges <- sum(
    source_counts$n[source_counts$source_api == "openalex"],
    na.rm = TRUE
  )
  duplicates <- seed_duplicate_counts(seeds)
  depth0 <- depth_counts$node_count[depth_counts$depth == 0L]
  depth1 <- depth_counts$node_count[depth_counts$depth == 1L]
  depth2 <- depth_counts$node_count[depth_counts$depth == 2L]
  post_direct <- if (nrow(top_seeds) > 0L) {
    sum(top_seeds$post_notice_direct_citers, na.rm = TRUE)
  } else 0L

  summary <- list(
    seed_records = as.integer(nrow(seeds)),
    resolved_seed_records = as.integer(
      sum(seeds$resolved_status == "resolved", na.rm = TRUE)
    ),
    linked_seed_records = as.integer(sum(!is.na(seeds$openalex_id))),
    resolved_seed_papers = as.integer(duplicates$resolved_seed_papers),
    duplicate_resolved_seed_records =
      as.integer(duplicates$duplicate_resolved_seed_records),
    duplicate_seed_rows_in_duplicate_groups =
      as.integer(duplicates$duplicate_seed_rows_in_duplicate_groups),
    depth2_nodes = as.integer(sum(depth_counts$node_count)),
    depth2_edges = as.integer(edge_scope$exported_edges),
    stored_edges = as.integer(edge_scope$stored_edges),
    edges_excluded_missing_source_node =
      as.integer(edge_scope$edges_excluded_missing_source_node),
    edges_excluded_missing_target_node =
      as.integer(edge_scope$edges_excluded_missing_target_node),
    depth0_nodes = as.integer(if (length(depth0)) depth0 else 0L),
    depth1_nodes = as.integer(if (length(depth1)) depth1 else 0L),
    depth2_nodes_only = as.integer(if (length(depth2)) depth2 else 0L),
    weak_components = "not_computed_large_graph_scale_limit",
    post_notice_direct_citers = as.integer(post_direct),
    opencitations_edges = as.integer(opencitations_edges),
    openalex_edges = as.integer(openalex_edges),
    full_graph_graphml_gz = graph_export$graphml_gz,
    full_graph_manifest = graph_export$manifest
  )
  summary_df <- tibble::tibble(
    metric = names(summary),
    value = as.character(unlist(summary, use.names = FALSE))
  )
  readr::write_csv(summary_df, file.path(table_dir, "summary.csv"))

  plot_depth_counts(depth_counts, figure_dir)
  plot_top_seeds(top_seeds, figure_dir)
  timeline <- large_post_notice_timeline(store$con)
  plot_post_notice_timeline_counts(timeline, figure_dir)

  summary
}

#' Copy a DuckDB query directly to a CSV path.
#' @noRd
duckdb_copy_query <- function(con, sql, path) {
  ensure_dir(dirname(path))
  normalized <- normalizePath(path, mustWork = FALSE)
  quoted_path <- paste0("'", gsub("'", "''", normalized, fixed = TRUE), "'")
  DBI::dbExecute(
    con,
    paste0("COPY (", sql, ") TO ", quoted_path,
           " (HEADER, DELIMITER ',')")
  )
  invisible(path)
}

#' Stream the full induced frontier graph to compressed GraphML.
#'
#' This avoids materializing the graph in igraph for full OpenCitations runs.
#' The output is a standards-shaped GraphML file compressed with gzip.
#'
#' @param con DBI connection.
#' @param graph_dir output graph directory.
#' @param max_analysis_depth integer depth included in the graph.
#' @param scale_counts optional counts from `analysis_scale_counts`.
#' @param chunk_size number of rows fetched per DBI chunk.
#' @return list with GraphML and manifest paths plus node/edge counts.
export_full_network_graph_graphml_gz <- function(
    con, graph_dir, max_analysis_depth = 2L, scale_counts = NULL,
    chunk_size = getOption("retractionpollution.graphml_chunk_size", 250000L)) {
  ensure_dir(graph_dir)
  depth <- as.integer(max_analysis_depth)
  graphml_gz <- file.path(graph_dir, "network_depth2.graphml.gz")
  manifest <- file.path(graph_dir, "network_depth2.graphml.json")
  temp_graphml <- paste0(graphml_gz, ".tmp")
  old_graphml <- file.path(graph_dir, "network_depth2.graphml")
  skip_note <- file.path(graph_dir, "network_depth2.graphml.skipped.txt")

  if (file.exists(old_graphml)) {
    stale_path <- file.path(
      graph_dir,
      paste0("network_depth2.graphml.stale-",
             format(Sys.time(), "%Y%m%d%H%M%S"))
    )
    file.rename(old_graphml, stale_path)
  }
  if (file.exists(skip_note)) file.remove(skip_note)
  if (file.exists(temp_graphml)) file.remove(temp_graphml)

  out <- gzfile(temp_graphml, open = "wt", compression = 1, encoding = "UTF-8")
  close_out <- TRUE
  on.exit({
    if (isTRUE(close_out)) close(out)
    if (file.exists(temp_graphml)) file.remove(temp_graphml)
  }, add = TRUE)

  writeLines(c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\">",
    "  <key id=\"n_depth\" for=\"node\" attr.name=\"depth\" attr.type=\"int\"/>",
    "  <key id=\"n_doi\" for=\"node\" attr.name=\"doi\" attr.type=\"string\"/>",
    "  <key id=\"n_title\" for=\"node\" attr.name=\"title\" attr.type=\"string\"/>",
    "  <key id=\"n_publication_year\" for=\"node\" attr.name=\"publication_year\" attr.type=\"int\"/>",
    "  <key id=\"n_work_type\" for=\"node\" attr.name=\"work_type\" attr.type=\"string\"/>",
    "  <key id=\"n_source_name\" for=\"node\" attr.name=\"source_name\" attr.type=\"string\"/>",
    "  <key id=\"n_topic_domain\" for=\"node\" attr.name=\"topic_domain\" attr.type=\"string\"/>",
    "  <key id=\"n_topic_name\" for=\"node\" attr.name=\"topic_name\" attr.type=\"string\"/>",
    "  <key id=\"e_depth\" for=\"edge\" attr.name=\"depth\" attr.type=\"int\"/>",
    "  <key id=\"e_source_api\" for=\"edge\" attr.name=\"source_api\" attr.type=\"string\"/>",
    "  <key id=\"e_citation_date\" for=\"edge\" attr.name=\"citation_date\" attr.type=\"string\"/>",
    "  <graph id=\"G\" edgedefault=\"directed\">"
  ), out, useBytes = TRUE)

  nodes_sql <- sprintf("
    SELECT f.openalex_id AS id, f.depth, w.doi, w.title,
           w.publication_year, w.work_type, w.source_name,
           w.topic_domain, w.topic_name
    FROM frontier_nodes f
    LEFT JOIN works w ON f.openalex_id = w.openalex_id
    WHERE f.depth <= %d
  ", depth)
  node_count <- stream_graphml_nodes(con, nodes_sql, out, chunk_size)

  edges_sql <- sprintf("
    SELECT e.source_id, e.target_id, e.depth, e.source_api, e.citation_date
    FROM citation_edges e
    JOIN frontier_nodes fs ON e.source_id = fs.openalex_id
    JOIN frontier_nodes ft ON e.target_id = ft.openalex_id
    WHERE fs.depth <= %d AND ft.depth <= %d
  ", depth, depth)
  edge_count <- stream_graphml_edges(con, edges_sql, out, chunk_size)

  writeLines(c("  </graph>", "</graphml>"), out, useBytes = TRUE)
  close(out)
  close_out <- FALSE
  file.rename(temp_graphml, graphml_gz)

  if (is.null(scale_counts)) {
    scale_counts <- analysis_scale_counts(
      list(con = con), max_analysis_depth = depth
    )
  }
  manifest_data <- list(
    format = "GraphML",
    compression = "gzip",
    graphml_gz = graphml_gz,
    max_analysis_depth = depth,
    node_count = as.integer(node_count),
    edge_count = as.integer(edge_count),
    expected_node_count = as.integer(scale_counts$nodes),
    expected_edge_count = as.integer(scale_counts$exported_edges),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    notes = paste(
      "Full induced frontier graph through depth 2.",
      "Depth 2 nodes are terminal descendants and are not expanded to depth 3."
    )
  )
  jsonlite::write_json(manifest_data, manifest, auto_unbox = TRUE,
                       pretty = TRUE)

  list(
    graphml_gz = graphml_gz,
    manifest = manifest,
    node_count = as.integer(node_count),
    edge_count = as.integer(edge_count)
  )
}

stream_graphml_nodes <- function(con, sql, out, chunk_size) {
  rs <- DBI::dbSendQuery(con, sql)
  on.exit(DBI::dbClearResult(rs), add = TRUE)
  total <- 0L
  repeat {
    chunk <- DBI::dbFetch(rs, n = as.integer(chunk_size))
    if (nrow(chunk) == 0L) break
    total <- total + nrow(chunk)
    id <- xml_escape_graphml(chunk$id)
    lines <- paste0(
      "    <node id=\"", id, "\">",
      graphml_data("n_depth", chunk$depth),
      graphml_data("n_doi", chunk$doi),
      graphml_data("n_title", chunk$title),
      graphml_data("n_publication_year", chunk$publication_year),
      graphml_data("n_work_type", chunk$work_type),
      graphml_data("n_source_name", chunk$source_name),
      graphml_data("n_topic_domain", chunk$topic_domain),
      graphml_data("n_topic_name", chunk$topic_name),
      "</node>"
    )
    writeLines(lines, out, useBytes = TRUE)
  }
  total
}

stream_graphml_edges <- function(con, sql, out, chunk_size) {
  rs <- DBI::dbSendQuery(con, sql)
  on.exit(DBI::dbClearResult(rs), add = TRUE)
  total <- 0L
  repeat {
    chunk <- DBI::dbFetch(rs, n = as.integer(chunk_size))
    if (nrow(chunk) == 0L) break
    n <- nrow(chunk)
    ids <- paste0("e", seq.int(total + 1L, total + n))
    lines <- paste0(
      "    <edge id=\"", ids,
      "\" source=\"", xml_escape_graphml(chunk$source_id),
      "\" target=\"", xml_escape_graphml(chunk$target_id), "\">",
      graphml_data("e_depth", chunk$depth),
      graphml_data("e_source_api", chunk$source_api),
      graphml_data("e_citation_date", chunk$citation_date),
      "</edge>"
    )
    writeLines(lines, out, useBytes = TRUE)
    total <- total + n
  }
  total
}

graphml_data <- function(key, value) {
  value <- xml_escape_graphml(value)
  missing <- is.na(value) | value == ""
  out <- paste0("<data key=\"", key, "\">", value, "</data>")
  out[missing] <- ""
  out
}

xml_escape_graphml <- function(value) {
  text <- as.character(value)
  text[is.na(value)] <- NA_character_
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&apos;", text, fixed = TRUE)
  text
}

#' Prepare temporary tables used by the large-analysis SQL metrics.
#' @noRd
create_large_analysis_temps <- function(con, canonical_ref, depth) {
  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE TEMP TABLE tmp_direct_seed_citers AS
    SELECT DISTINCT cs.record_id, e.source_id, cs.notice_date
    FROM citation_edges e
    JOIN %s cs ON e.target_id = cs.openalex_id
    JOIN frontier_nodes fs ON e.source_id = fs.openalex_id
    WHERE e.depth = 1 AND fs.depth <= %d
  ", canonical_ref, depth))
  DBI::dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE tmp_seed_depth2_citers AS
    SELECT DISTINCT d.record_id, e.source_id
    FROM tmp_direct_seed_citers d
    JOIN citation_edges e ON e.target_id = d.source_id
    JOIN frontier_nodes fs ON e.source_id = fs.openalex_id AND fs.depth = 2
    WHERE e.depth = 2
  ")
  invisible(NULL)
}

#' Compute seed metrics with DuckDB temp tables.
#' @noRd
large_seed_metrics <- function(con, canonical_ref) {
  DBI::dbGetQuery(con, sprintf("
    WITH direct_counts AS (
      SELECT record_id, COUNT(DISTINCT source_id) AS direct_citers
      FROM tmp_direct_seed_citers
      GROUP BY record_id
    ),
    d2_counts AS (
      SELECT record_id, COUNT(DISTINCT source_id) AS depth2_descendants
      FROM tmp_seed_depth2_citers
      GROUP BY record_id
    ),
    total_reach AS (
      SELECT record_id, COUNT(DISTINCT source_id) AS total_depth2_reach
      FROM (
        SELECT record_id, source_id FROM tmp_direct_seed_citers
        UNION
        SELECT record_id, source_id FROM tmp_seed_depth2_citers
      )
      GROUP BY record_id
    ),
    post_direct AS (
      SELECT d.record_id,
             COUNT(DISTINCT d.source_id) AS post_notice_direct_citers
      FROM tmp_direct_seed_citers d
      JOIN works w ON d.source_id = w.openalex_id
      WHERE d.notice_date IS NOT NULL
        AND w.publication_date IS NOT NULL
        AND w.publication_date > d.notice_date
      GROUP BY d.record_id
    ),
    post_d2 AS (
      SELECT d2.record_id,
             COUNT(DISTINCT d2.source_id) AS post_notice_depth2_descendants
      FROM tmp_seed_depth2_citers d2
      JOIN %s cs ON d2.record_id = cs.record_id
      JOIN works w ON d2.source_id = w.openalex_id
      WHERE cs.notice_date IS NOT NULL
        AND w.publication_date IS NOT NULL
        AND w.publication_date > cs.notice_date
      GROUP BY d2.record_id
    )
    SELECT cs.record_id, cs.seed_record_count, cs.seed_record_ids,
           cs.openalex_id, cs.title, cs.notice_type, cs.notice_date,
           COALESCE(dc.direct_citers, 0) AS direct_citers,
           COALESCE(d2.depth2_descendants, 0) AS depth2_descendants,
           COALESCE(tr.total_depth2_reach, 0) AS total_depth2_reach,
           COALESCE(pd.post_notice_direct_citers, 0) AS post_notice_direct_citers,
           COALESCE(p2.post_notice_depth2_descendants, 0) AS post_notice_depth2_descendants
    FROM %s cs
    LEFT JOIN direct_counts dc ON cs.record_id = dc.record_id
    LEFT JOIN d2_counts d2 ON cs.record_id = d2.record_id
    LEFT JOIN total_reach tr ON cs.record_id = tr.record_id
    LEFT JOIN post_direct pd ON cs.record_id = pd.record_id
    LEFT JOIN post_d2 p2 ON cs.record_id = p2.record_id
    ORDER BY total_depth2_reach DESC
  ", canonical_ref, canonical_ref))
}

#' Count post-notice direct citations by publication year for large analysis.
#' @noRd
large_post_notice_timeline <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT CAST(w.publication_year AS INTEGER) AS year,
           COUNT(*) AS count
    FROM tmp_direct_seed_citers d
    JOIN works w ON d.source_id = w.openalex_id
    WHERE d.notice_date IS NOT NULL
      AND w.publication_date IS NOT NULL
      AND w.publication_date > d.notice_date
      AND w.publication_year IS NOT NULL
    GROUP BY year
    ORDER BY year
  ")
}

#' Plot post-notice counts that have already been aggregated.
#' @noRd
plot_post_notice_timeline_counts <- function(counts, figure_dir) {
  png <- file.path(figure_dir, "post_notice_timeline.png")
  svg <- file.path(figure_dir, "post_notice_timeline.svg")
  if (is.null(counts) || nrow(counts) == 0L) {
    if (file.exists(png)) file.remove(png)
    if (file.exists(svg)) file.remove(svg)
    return(invisible(NULL))
  }
  p <- ggplot2::ggplot(counts, ggplot2::aes(x = .data$year, y = .data$count)) +
    ggplot2::geom_line(color = "#0f766e") +
    ggplot2::geom_point(color = "#0f766e") +
    ggplot2::labs(x = "Publication year",
                  y = "Post-notice direct citations",
                  title = "Post-Notice Direct Citations Over Time") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(png, p, width = 8, height = 4, dpi = 200)
  ggplot2::ggsave(svg, p, width = 8, height = 4)
  invisible(NULL)
}

#' Compute per-seed pollution metrics using vectorized dplyr joins.
#'
#' @param seeds tibble of seed rows (from `seeds` table).
#' @param nodes tibble of frontier nodes joined to works.
#' @param edges tibble of citation edges.
#' @return tibble with one row per resolved seed, sorted by
#'   `total_depth2_reach` descending.
seed_metrics <- function(seeds, nodes, edges) {
  cols <- c("record_id", "seed_record_count", "seed_record_ids",
            "openalex_id", "title", "notice_type", "notice_date",
            "direct_citers", "depth2_descendants", "total_depth2_reach",
            "post_notice_direct_citers", "post_notice_depth2_descendants")
  if (nrow(seeds) == 0L || nrow(nodes) == 0L || nrow(edges) == 0L) {
    return(tibble::tibble(
      record_id = character(), seed_record_count = integer(),
      seed_record_ids = character(),
      openalex_id = character(), title = character(),
      notice_type = character(), notice_date = as.Date(character()),
      direct_citers = integer(), depth2_descendants = integer(),
      total_depth2_reach = integer(),
      post_notice_direct_citers = integer(),
      post_notice_depth2_descendants = integer()
    ))
  }

  sx <- canonical_seed_records(seeds)

  if (nrow(sx) == 0L) {
    return(tibble::tibble(
      record_id = character(), seed_record_count = integer(),
      seed_record_ids = character(),
      openalex_id = character(), title = character(),
      notice_type = character(), notice_date = as.Date(character()),
      direct_citers = integer(), depth2_descendants = integer(),
      total_depth2_reach = integer(),
      post_notice_direct_citers = integer(),
      post_notice_depth2_descendants = integer()
    ))
  }

  # Ensure notice_date is a Date.
  sx <- sx |>
    dplyr::mutate(notice_date = as.Date(notice_date))

  node_pub <- nodes |>
    dplyr::select(openalex_id, publication_date, depth) |>
    dplyr::mutate(publication_date = as.Date(publication_date))

  # Edges targeting a seed: each row is (record_id, source_id=citer, target_id=seed, notice_date)
  direct <- edges |>
    dplyr::inner_join(sx, by = c("target_id" = "openalex_id")) |>
    dplyr::select(record_id, source_id, notice_date) |>
    dplyr::distinct()

  direct_counts <- direct |>
    dplyr::group_by(record_id) |>
    dplyr::summarise(direct_citers = dplyr::n_distinct(source_id),
                     .groups = "drop")

  # Depth-2 descendants: edges targeting a direct citer, source at depth 2.
  edges_d2 <- edges |> dplyr::filter(depth == 2L)
  d2_src <- direct |>
    dplyr::select(record_id, citer_id = source_id) |>
    dplyr::distinct() |>
    dplyr::inner_join(edges_d2, by = c("citer_id" = "target_id"),
                      relationship = "many-to-many") |>
    dplyr::select(record_id, source_id) |>
    dplyr::distinct()

  d2_counts <- d2_src |>
    dplyr::group_by(record_id) |>
    dplyr::summarise(depth2_descendants = dplyr::n_distinct(source_id),
                     .groups = "drop")

  # Union reach: distinct (record_id, source_id) across direct and d2.
  total_reach <- dplyr::bind_rows(
    direct |> dplyr::select(record_id, source_id) |> dplyr::distinct(),
    d2_src
  ) |>
    dplyr::distinct() |>
    dplyr::group_by(record_id) |>
    dplyr::summarise(total_depth2_reach = dplyr::n(), .groups = "drop")

  # Post-notice direct citers.
  post_direct <- direct |>
    dplyr::inner_join(node_pub |> dplyr::select(openalex_id, publication_date),
                      by = c("source_id" = "openalex_id")) |>
    dplyr::filter(!is.na(notice_date), !is.na(publication_date),
                  publication_date > notice_date) |>
    dplyr::group_by(record_id) |>
    dplyr::summarise(post_notice_direct_citers = dplyr::n_distinct(source_id),
                     .groups = "drop")

  # Post-notice depth-2 descendants.
  post_d2 <- d2_src |>
    dplyr::inner_join(sx |> dplyr::select(record_id, notice_date),
                      by = "record_id") |>
    dplyr::inner_join(node_pub |> dplyr::select(openalex_id, publication_date),
                      by = c("source_id" = "openalex_id")) |>
    dplyr::filter(!is.na(notice_date), !is.na(publication_date),
                  publication_date > notice_date) |>
    dplyr::group_by(record_id) |>
    dplyr::summarise(post_notice_depth2_descendants = dplyr::n_distinct(source_id),
                     .groups = "drop")

  out <- sx |>
    dplyr::left_join(direct_counts, by = "record_id") |>
    dplyr::left_join(d2_counts, by = "record_id") |>
    dplyr::left_join(total_reach, by = "record_id") |>
    dplyr::left_join(post_direct, by = "record_id") |>
    dplyr::left_join(post_d2, by = "record_id") |>
    dplyr::mutate(
      direct_citers = tidyr::replace_na(direct_citers, 0L),
      depth2_descendants = tidyr::replace_na(depth2_descendants, 0L),
      total_depth2_reach = tidyr::replace_na(total_depth2_reach, 0L),
      post_notice_direct_citers = tidyr::replace_na(post_notice_direct_citers, 0L),
      post_notice_depth2_descendants = tidyr::replace_na(post_notice_depth2_descendants, 0L)
    ) |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::arrange(dplyr::desc(total_depth2_reach))

  out
}

#' Compute bridge-paper metrics for depth-1 nodes, vectorized.
#'
#' @param nodes tibble.
#' @param edges tibble.
#' @return tibble with `openalex_id`, `title`, `cited_seed_count`,
#'   `depth2_citer_count`, sorted by depth2_citer_count then cited_seed_count
#'   descending.
bridge_metrics <- function(nodes, edges) {
  if (nrow(nodes) == 0L || nrow(edges) == 0L) {
    return(tibble::tibble(openalex_id = character(), title = character(),
                          cited_seed_count = integer(),
                          depth2_citer_count = integer()))
  }

  depth0_ids <- nodes |>
    dplyr::filter(depth == 0L) |>
    dplyr::select(openalex_id) |>
    dplyr::distinct()
  depth2_ids <- nodes |>
    dplyr::filter(depth == 2L) |>
    dplyr::select(openalex_id) |>
    dplyr::distinct()
  depth1_nodes <- nodes |>
    dplyr::filter(depth == 1L) |>
    dplyr::select(openalex_id, title)

  cited_seed <- edges |>
    dplyr::filter(depth == 1L) |>
    dplyr::inner_join(depth0_ids, by = c("target_id" = "openalex_id")) |>
    dplyr::count(source_id, name = "cited_seed_count")

  depth2_citer <- edges |>
    dplyr::filter(depth == 2L) |>
    dplyr::inner_join(depth2_ids, by = c("source_id" = "openalex_id")) |>
    dplyr::count(target_id, name = "depth2_citer_count")

  out <- depth1_nodes |>
    dplyr::full_join(cited_seed, by = c("openalex_id" = "source_id")) |>
    dplyr::full_join(depth2_citer, by = c("openalex_id" = "target_id")) |>
    dplyr::mutate(
      cited_seed_count = tidyr::replace_na(cited_seed_count, 0L),
      depth2_citer_count = tidyr::replace_na(depth2_citer_count, 0L)
    ) |>
    dplyr::filter(cited_seed_count > 0L | depth2_citer_count > 0L) |>
    dplyr::select(openalex_id, title, cited_seed_count, depth2_citer_count) |>
    dplyr::arrange(dplyr::desc(depth2_citer_count), dplyr::desc(cited_seed_count))

  out
}

#' Build a directed igraph from nodes/edges.
#'
#' @param nodes tibble with `openalex_id` as first column (vertex names).
#' @param edges tibble with `source_id`, `target_id`.
#' @return igraph directed graph.
build_graph <- function(nodes, edges) {
  v <- nodes |>
    dplyr::select(openalex_id, dplyr::everything()) |>
    as.data.frame()
  e <- edges |>
    as.data.frame()
  if (nrow(e) == 0L) {
    g <- igraph::make_empty_graph(directed = TRUE)
    if (nrow(v) > 0L) {
      g <- igraph::add_vertices(g, nrow(v), name = v$openalex_id)
    }
    return(g)
  }
  igraph::graph_from_data_frame(d = e, vertices = v, directed = TRUE)
}

#' Summarize the analysis graph and metrics.
#'
#' @param graph igraph.
#' @param seeds tibble.
#' @param nodes tibble.
#' @param edges tibble.
#' @param top_seeds tibble (result of `seed_metrics`).
#' @return named list of summary metrics.
#' @noRd
summary_metrics <- function(graph, seeds, nodes, edges, top_seeds,
                            edge_scope = NULL) {
  n_seeds <- nrow(seeds)
  linked <- if (n_seeds > 0L) sum(!is.na(seeds$openalex_id)) else 0L
  resolved <- if (n_seeds > 0L && "resolved_status" %in% names(seeds)) {
    sum(seeds$resolved_status == "resolved", na.rm = TRUE)
  } else linked
  duplicates <- seed_duplicate_counts(seeds)
  post_direct <- if (!is.null(top_seeds) && nrow(top_seeds) > 0L &&
                     "post_notice_direct_citers" %in% names(top_seeds)) {
    sum(top_seeds$post_notice_direct_citers, na.rm = TRUE)
  } else 0L
  weak_no <- if (!is.null(graph) && igraph::vcount(graph) > 0L) {
    igraph::components(graph, mode = "weak")$no
  } else 0L
  oc <- if (nrow(edges) > 0L && "source_api" %in% names(edges)) {
    sum(edges$source_api == "opencitations", na.rm = TRUE)
  } else 0L
  oa <- if (nrow(edges) > 0L && "source_api" %in% names(edges)) {
    sum(edges$source_api == "openalex", na.rm = TRUE)
  } else 0L
  stored_edges <- if (!is.null(edge_scope) && "stored_edges" %in% names(edge_scope)) {
    edge_scope$stored_edges
  } else nrow(edges)
  excluded_source <- if (!is.null(edge_scope) &&
                         "edges_excluded_missing_source_node" %in% names(edge_scope)) {
    edge_scope$edges_excluded_missing_source_node
  } else 0L
  excluded_target <- if (!is.null(edge_scope) &&
                         "edges_excluded_missing_target_node" %in% names(edge_scope)) {
    edge_scope$edges_excluded_missing_target_node
  } else 0L
  list(
    seed_records = as.integer(n_seeds),
    resolved_seed_records = as.integer(resolved),
    linked_seed_records = as.integer(linked),
    resolved_seed_papers = as.integer(duplicates$resolved_seed_papers),
    duplicate_resolved_seed_records =
      as.integer(duplicates$duplicate_resolved_seed_records),
    duplicate_seed_rows_in_duplicate_groups =
      as.integer(duplicates$duplicate_seed_rows_in_duplicate_groups),
    depth2_nodes = as.integer(nrow(nodes)),
    depth2_edges = as.integer(nrow(edges)),
    stored_edges = as.integer(stored_edges),
    edges_excluded_missing_source_node = as.integer(excluded_source),
    edges_excluded_missing_target_node = as.integer(excluded_target),
    depth0_nodes = as.integer(if (nrow(nodes) > 0L) sum(nodes$depth == 0L, na.rm = TRUE) else 0L),
    depth1_nodes = as.integer(if (nrow(nodes) > 0L) sum(nodes$depth == 1L, na.rm = TRUE) else 0L),
    depth2_nodes_only = as.integer(if (nrow(nodes) > 0L) sum(nodes$depth == 2L, na.rm = TRUE) else 0L),
    weak_components = as.integer(weak_no),
    post_notice_direct_citers = as.integer(post_direct),
    opencitations_edges = as.integer(oc),
    openalex_edges = as.integer(oa)
  )
}

#' Count stored edges included or excluded by the analysis frontier join.
#' @param store StudyStore.
#' @param max_analysis_depth integer.
#' @return named list of integer counts.
analysis_edge_scope <- function(store, max_analysis_depth = 2L) {
  sql <- "
    WITH frontier AS (
      SELECT openalex_id
      FROM frontier_nodes
      WHERE depth <= ?
    ),
    classified AS (
      SELECT
        e.source_id,
        e.target_id,
        fs.openalex_id AS source_node,
        ft.openalex_id AS target_node
      FROM citation_edges e
      LEFT JOIN frontier fs ON e.source_id = fs.openalex_id
      LEFT JOIN frontier ft ON e.target_id = ft.openalex_id
    )
    SELECT
      COUNT(*) AS stored_edges,
      SUM(source_node IS NULL) AS edges_excluded_missing_source_node,
      SUM(target_node IS NULL) AS edges_excluded_missing_target_node,
      SUM(source_node IS NOT NULL AND target_node IS NOT NULL) AS exported_edges
    FROM classified
  "
  df <- DBI::dbGetQuery(store$con, sql,
                        params = list(as.integer(max_analysis_depth)))
  list(
    stored_edges = as.integer(df$stored_edges[1]),
    edges_excluded_missing_source_node =
      as.integer(df$edges_excluded_missing_source_node[1]),
    edges_excluded_missing_target_node =
      as.integer(df$edges_excluded_missing_target_node[1]),
    exported_edges = as.integer(df$exported_edges[1])
  )
}

#' Bar chart of node count by depth.
#' @noRd
plot_depth_counts <- function(depth_counts, figure_dir) {
  if (nrow(depth_counts) == 0L) return(invisible(NULL))
  p <- ggplot2::ggplot(depth_counts,
                       ggplot2::aes(x = factor(.data$depth), y = .data$node_count)) +
    ggplot2::geom_col(fill = "#3b82f6") +
    ggplot2::labs(x = "Network depth", y = "Nodes",
                  title = "Citation Pollution Network Size by Depth") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(figure_dir, "depth_counts.png"), p,
                  width = 6, height = 4, dpi = 200)
  ggplot2::ggsave(file.path(figure_dir, "depth_counts.svg"), p,
                  width = 6, height = 4)
  invisible(NULL)
}

#' Horizontal bar chart of the top polluted seeds.
#' @noRd
plot_top_seeds <- function(top_seeds, figure_dir) {
  if (is.null(top_seeds) || nrow(top_seeds) == 0L) return(invisible(NULL))
  data <- top_seeds |>
    utils::head(15) |>
    dplyr::mutate(label = ifelse(is.na(title), "", as.character(title)),
                  label = substr(.data$label, 1L, 70L),
                  rank = dplyr::row_number(),
                  label = sprintf("%03d. %s", .data$rank, .data$label),
                  label = factor(.data$label, levels = .data$label))
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$label,
                                          y = .data$total_depth2_reach)) +
    ggplot2::geom_col(fill = "#ef4444") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Unique descendants through depth 2",
                  title = "Most Polluted Retracted or Concerning Papers") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(figure_dir, "top_polluted_seeds.png"), p,
                  width = 9, height = 6, dpi = 200)
  ggplot2::ggsave(file.path(figure_dir, "top_polluted_seeds.svg"), p,
                  width = 9, height = 6)
  invisible(NULL)
}

#' Line chart of post-notice direct citation counts by publication year.
#' @noRd
plot_post_notice_timeline <- function(seeds, nodes, edges, figure_dir) {
  if (nrow(seeds) == 0L || nrow(nodes) == 0L || nrow(edges) == 0L) {
    return(invisible(NULL))
  }
  sx <- canonical_seed_records(seeds) |>
    dplyr::select(openalex_id, notice_date) |>
    dplyr::mutate(notice_date = as.Date(notice_date))
  node_pub <- nodes |>
    dplyr::select(openalex_id, publication_date, publication_year) |>
    dplyr::mutate(publication_date = as.Date(publication_date))

  # Edges targeting a seed; source is the citer.
  rows <- edges |>
    dplyr::inner_join(sx, by = c("target_id" = "openalex_id")) |>
    dplyr::inner_join(node_pub, by = c("source_id" = "openalex_id"),
                      suffix = c("", "_src")) |>
    dplyr::filter(!is.na(notice_date), !is.na(publication_date),
                  publication_date > notice_date, !is.na(publication_year))

  if (nrow(rows) == 0L) return(invisible(NULL))

  counts <- rows |>
    dplyr::mutate(year = as.integer(.data$publication_year)) |>
    dplyr::count(year, name = "count") |>
    dplyr::arrange(year)

  p <- ggplot2::ggplot(counts, ggplot2::aes(x = .data$year, y = .data$count)) +
    ggplot2::geom_line(color = "#0f766e") +
    ggplot2::geom_point(color = "#0f766e") +
    ggplot2::labs(x = "Publication year",
                  y = "Post-notice direct citations",
                  title = "Post-Notice Direct Citations Over Time") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(figure_dir, "post_notice_timeline.png"), p,
                  width = 8, height = 4, dpi = 200)
  ggplot2::ggsave(file.path(figure_dir, "post_notice_timeline.svg"), p,
                  width = 8, height = 4)
  invisible(NULL)
}
