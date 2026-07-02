#' @title Reproducible manuscript statistics, figures, and rendering
#' @description Generates every headline number and every manuscript figure
#'   directly from the study store and the analysis output tables, then renders
#'   the parameterized Quarto manuscript. Before this module the manuscript and
#'   its figures were produced by ad-hoc, uncommitted code, so the primary
#'   scientific artifact was not reproducible (audit finding C1). This module
#'   closes that gap: `build_manuscript()` derives all numbers from the DuckDB
#'   store plus `tables/top_polluted_seeds.csv`, writes `manuscript_stats.json`,
#'   renders the figures with committed ggplot code, and renders the manuscript
#'   to HTML/PDF/DOCX/GFM.
#'
#'   It also fixes the reporting defects the ad-hoc manuscript carried:
#'   * R1: "post-notice depth-2 descendants" was a per-seed sum with multiplicity
#'     that exceeded the number of depth-2 nodes. This module reports the unique
#'     descendant-node count alongside the per-seed relationship sum.
#'   * R2: "post-notice direct citers" was likewise a per-seed sum; the unique
#'     node count is reported.
#'   * R3/R4: the uninterpretable degree-ranked density figure and the log-scale
#'     two-bar chart are replaced by a reach survival + concentration figure and
#'     an event-study aligned to the notice year.

library(dplyr)
library(ggplot2)

#' Null/NA-coalescing helper.
#' @param a value to test.
#' @param b fallback.
#' @return `a` unless it is NULL/empty/NA, otherwise `b`.
#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a[1]))) b else a
}

#' Gini coefficient via the sorted-order formula (O(n log n)).
#' @param x non-negative numeric vector.
#' @return scalar Gini coefficient, or `NA_real_` when undefined.
manuscript_gini <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) return(NA_real_)
  s <- sum(x)
  if (s == 0) return(0)
  xs <- sort(x)
  g <- (2 * sum(seq_len(n) * xs)) / (n * s) - (n + 1) / n
  as.numeric(g)
}

#' Read a metadata JSON value from the store, parsed to a list.
#' @param store StudyStore.
#' @param key character metadata key.
#' @return list (empty on missing/parse error).
manuscript_metadata_json <- function(store, key) {
  raw <- store$get_metadata(key, "")
  if (is.na(raw) || !nzchar(raw)) return(list())
  tryCatch(jsonlite::fromJSON(raw), error = function(e) list())
}

#' Compute every manuscript statistic from the store and analysis tables.
#'
#' Seed-level distribution, concentration, and top-seed statistics are read from
#' the committed `top_polluted_seeds.csv`; node/edge/reconciliation counts and
#' the unique post-notice node counts are queried from the DuckDB store.
#'
#' @param store StudyStore.
#' @param tables_dir directory containing `top_polluted_seeds.csv` etc.
#' @param max_analysis_depth integer depth included in analysis.
#' @param compute_unique_post_d2 logical; the unique post-notice depth-2 count
#'   requires a two-hop scan over all edges. Set `FALSE` to skip it.
#' @return named list of scalars and small data frames.
compute_manuscript_stats <- function(store, tables_dir, max_analysis_depth = 2L,
                                     compute_unique_post_d2 = TRUE) {
  con <- store$con
  depth <- as.integer(max_analysis_depth)

  seeds <- tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM seeds"))
  seed_stats <- seed_duplicate_counts(seeds)

  # --- Depth counts (authoritative, from the frontier) ---------------------
  depth_counts <- DBI::dbGetQuery(con, sprintf(
    "SELECT depth, COUNT(*) AS node_count FROM frontier_nodes
     WHERE depth <= %d GROUP BY depth ORDER BY depth", depth))
  depth_of <- function(d) {
    v <- depth_counts$node_count[depth_counts$depth == d]
    if (length(v)) as.numeric(v[1]) else 0
  }
  total_nodes <- sum(depth_counts$node_count)

  # --- Edge counts by depth (induced frontier) -----------------------------
  edge_depths <- DBI::dbGetQuery(con, sprintf(
    "WITH fr AS (SELECT openalex_id FROM frontier_nodes WHERE depth <= %d)
     SELECT e.depth AS depth, e.source_api AS source_api, COUNT(*) AS edge_count,
            COUNT(DISTINCT e.source_id) AS distinct_sources,
            COUNT(DISTINCT e.target_id) AS distinct_targets
     FROM citation_edges e
     JOIN fr s ON e.source_id = s.openalex_id
     JOIN fr t ON e.target_id = t.openalex_id
     GROUP BY e.depth, e.source_api ORDER BY e.depth", depth))
  edge_of <- function(d) {
    v <- edge_depths$edge_count[edge_depths$depth == d]
    if (length(v)) as.numeric(sum(v)) else 0
  }
  total_edges <- sum(edge_depths$edge_count)

  # --- Seed-level reach distribution (from committed analysis table) --------
  seeds_csv <- file.path(tables_dir, "top_polluted_seeds.csv")
  top <- readr::read_csv(seeds_csv, show_col_types = FALSE, progress = FALSE)
  reach <- top$total_depth2_reach
  n_seed <- nrow(top)
  q <- stats::quantile(reach, c(0.5, 0.9, 0.95, 0.99), names = FALSE)
  reach_stats <- list(
    seed_papers = n_seed,
    reach_median = as.numeric(q[1]),
    reach_mean = mean(reach),
    reach_p90 = as.numeric(q[2]),
    reach_p95 = as.numeric(q[3]),
    reach_p99 = as.numeric(q[4]),
    reach_max = max(reach),
    zero_reach_n = sum(reach == 0),
    zero_reach_pct = 100 * mean(reach == 0),
    reach_gini = manuscript_gini(as.numeric(reach))
  )

  # Per-seed post-notice sums (relationship counts, multiplicity per seed).
  post_direct_sum <- sum(top$post_notice_direct_citers, na.rm = TRUE)
  post_d2_sum <- sum(top$post_notice_depth2_descendants, na.rm = TRUE)
  seeds_with_post_direct <- sum(top$post_notice_direct_citers > 0, na.rm = TRUE)
  seeds_with_post_d2 <- sum(top$post_notice_depth2_descendants > 0, na.rm = TRUE)

  top10 <- utils::head(top, 10L)

  bridge_csv <- file.path(tables_dir, "bridge_papers.csv")
  bridge_top10 <- if (file.exists(bridge_csv)) {
    readr::read_csv(bridge_csv, n_max = 10L, show_col_types = FALSE,
                    progress = FALSE)
  } else {
    tibble::tibble(openalex_id = character(), title = character(),
                   cited_seed_count = integer(), depth2_citer_count = integer())
  }

  # --- Unique post-notice node counts (fixes R1/R2) ------------------------
  unique_post_direct <- DBI::dbGetQuery(con, "
    WITH canon AS (
      SELECT openalex_id, MIN(notice_date) AS nd
      FROM seeds WHERE openalex_id IS NOT NULL AND resolved_status = 'resolved'
      GROUP BY openalex_id
    )
    SELECT COUNT(DISTINCT e.source_id) AS n
    FROM citation_edges e
    JOIN canon c ON e.target_id = c.openalex_id
    JOIN works w ON e.source_id = w.openalex_id
    WHERE e.depth = 1 AND w.publication_date IS NOT NULL
      AND c.nd IS NOT NULL AND w.publication_date > c.nd")$n[1]

  unique_post_d2 <- NA_real_
  if (isTRUE(compute_unique_post_d2)) {
    canonical <- canonical_seed_records(seeds)
    tmp <- paste0("tmp_ms_canon_", Sys.getpid())
    DBI::dbWriteTable(con, tmp, canonical, temporary = TRUE, overwrite = TRUE)
    ref <- DBI::dbQuoteIdentifier(con, tmp)
    create_large_analysis_temps(con, ref, depth)
    unique_post_d2 <- DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(DISTINCT d2.source_id) AS n
      FROM tmp_seed_depth2_citers d2
      JOIN %s cs ON d2.record_id = cs.record_id
      JOIN works w ON d2.source_id = w.openalex_id
      WHERE cs.notice_date IS NOT NULL AND w.publication_date IS NOT NULL
        AND w.publication_date > cs.notice_date", ref))$n[1]
  }

  # --- Event study: direct citations aligned to the notice year ------------
  event <- DBI::dbGetQuery(con, "
    WITH canon AS (
      SELECT openalex_id,
             CAST(EXTRACT(year FROM MIN(notice_date)) AS INTEGER) AS ny
      FROM seeds WHERE openalex_id IS NOT NULL AND resolved_status = 'resolved'
      GROUP BY openalex_id
    )
    SELECT (CAST(w.publication_year AS INTEGER) - c.ny) AS offset_year,
           COUNT(*) AS n
    FROM citation_edges e
    JOIN canon c ON e.target_id = c.openalex_id
    JOIN works w ON e.source_id = w.openalex_id
    WHERE e.depth = 1 AND w.publication_year IS NOT NULL AND c.ny IS NOT NULL
    GROUP BY offset_year ORDER BY offset_year")

  # --- Coverage (for honest lower-bound disclosure) ------------------------
  cov <- DBI::dbGetQuery(con, sprintf("
    SELECT f.depth AS depth, COUNT(*) AS nodes,
           SUM(CASE WHEN w.publication_date IS NULL THEN 1 ELSE 0 END) AS pubdate_null
    FROM frontier_nodes f LEFT JOIN works w ON f.openalex_id = w.openalex_id
    WHERE f.depth IN (1, 2) AND f.depth <= %d GROUP BY f.depth ORDER BY f.depth",
    depth))

  # --- Provenance + reconciliation (from run_metadata) ---------------------
  bulk <- manuscript_metadata_json(store, "opencitations_bulk_completion")
  rest <- manuscript_metadata_json(store, "opencitations_rest_reconciliation")
  prune <- manuscript_metadata_json(store, "opencitations_zero_count_prune")
  stale <- manuscript_metadata_json(store, "opencitations_stale_edge_cleanup")
  seed_meta <- manuscript_metadata_json(store, "opencitations_seed_stats")

  list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    rw_snapshot_date = store$get_metadata("rw_snapshot_date", "unknown"),
    oc_access_date = store$get_metadata("oc_access_date", "unknown"),
    pipeline_mode = store$get_metadata("pipeline_mode", "unknown"),

    seed_records = as.integer(nrow(seeds)),
    linked_seed_records = as.integer(sum(!is.na(seeds$openalex_id))),
    resolved_seed_records =
      as.integer(sum(seeds$resolved_status == "resolved", na.rm = TRUE)),
    resolved_seed_papers = as.integer(seed_stats$resolved_seed_papers),
    duplicate_resolved_seed_records =
      as.integer(seed_stats$duplicate_resolved_seed_records),
    duplicate_seed_rows_in_duplicate_groups =
      as.integer(seed_stats$duplicate_seed_rows_in_duplicate_groups),
    no_doi_seeds = as.integer(seed_meta$no_doi %||% 0L),

    depth0_nodes = depth_of(0),
    depth1_nodes = depth_of(1),
    depth2_nodes = depth_of(2),
    total_nodes = total_nodes,
    total_edges = total_edges,
    depth1_edges = edge_of(1),
    depth2_edges = edge_of(2),
    depth1_distinct_sources =
      as.numeric(sum(edge_depths$distinct_sources[edge_depths$depth == 1])),
    depth1_distinct_targets =
      as.numeric(sum(edge_depths$distinct_targets[edge_depths$depth == 1])),
    depth2_distinct_sources =
      as.numeric(sum(edge_depths$distinct_sources[edge_depths$depth == 2])),
    depth2_distinct_targets =
      as.numeric(sum(edge_depths$distinct_targets[edge_depths$depth == 2])),

    reach = reach_stats,
    post_notice = list(
      direct_relationship_sum = as.numeric(post_direct_sum),
      direct_unique_nodes = as.numeric(unique_post_direct),
      depth2_relationship_sum = as.numeric(post_d2_sum),
      depth2_unique_nodes = as.numeric(unique_post_d2),
      seeds_with_post_direct = as.integer(seeds_with_post_direct),
      seeds_with_post_depth2 = as.integer(seeds_with_post_d2)
    ),

    depth1_pubdate_null =
      as.numeric(sum(cov$pubdate_null[cov$depth == 1])),
    depth2_pubdate_null =
      as.numeric(sum(cov$pubdate_null[cov$depth == 2])),

    reconciliation = list(
      nonzero_depth1_parents =
        as.numeric(bulk$citation_count_dump$nonzero_depth1_parent_rows %||% NA),
      bulk_extracted_rows =
        as.numeric(bulk$full_index_dump$extracted_bulk_citation_rows %||% NA),
      unique_citing_omids =
        as.numeric(bulk$omid_mapping$unique_citing_omids %||% NA),
      unique_citing_omids_mapped =
        as.numeric(bulk$omid_mapping$unique_citing_omids_mapped_to_doi_or_pmid %||% NA),
      exact_parents = as.numeric(rest$exact_parent_count %||% NA),
      undercount_parents = as.numeric(rest$undercount_parent_count %||% NA),
      overcount_parents = as.numeric(rest$overcount_parent_count %||% NA),
      overcount_extra_citations =
        as.numeric(rest$overcount_extra_total_against_citation_count_dump %||% NA),
      net_delta_citations =
        as.numeric(rest$net_delta_against_citation_count_dump %||% NA),
      rest_parents_processed = as.numeric(rest$targeted_rest_processed %||% NA),
      rest_failures = as.numeric(rest$targeted_rest_failed %||% NA),
      rest_rows = as.numeric(rest$targeted_rest_citations_returned %||% NA),
      zero_count_pruned = as.numeric(prune$zero_depth1_parents_marked %||% NA),
      non_doi_terminal = as.numeric(prune$non_doi_depth1_parents_marked %||% NA),
      stale_edges_deleted = as.numeric(stale$edges_deleted %||% NA),
      stale_missing_targets = as.numeric(stale$unique_missing_targets %||% NA)
    ),

    depth_counts = depth_counts,
    edge_depths = edge_depths,
    top10 = top10,
    bridge_top10 = bridge_top10,
    reach_vector = as.numeric(reach),
    direct_vector = as.numeric(top$direct_citers),
    depth2_vector = as.numeric(top$depth2_descendants),
    event = event
  )
}

#' Save a ggplot as both PNG and SVG.
#' @param plot ggplot.
#' @param path_no_ext character path without extension.
#' @param width,height numeric inches.
#' @return invisible NULL.
manuscript_save <- function(plot, path_no_ext, width = 8, height = 5) {
  ggplot2::ggsave(paste0(path_no_ext, ".png"), plot, width = width,
                  height = height, dpi = 200, bg = "white")
  ggplot2::ggsave(paste0(path_no_ext, ".svg"), plot, width = width,
                  height = height, bg = "white")
  invisible(NULL)
}

MS_THEME <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

#' Comma-formatted integer label.
#' @noRd
ms_comma <- function(x) formatC(x, format = "d", big.mark = ",")

#' Build all six manuscript figures from computed stats.
#'
#' Figures are numbered to match the manuscript body (fixes R5 file/number
#' scrambling): 1 depth counts, 2 reach survival + concentration (replaces the
#' density hairball), 3 edge counts by depth, 4 reach histogram, 5 top-seed
#' neighborhoods, 6 post-notice event study (replaces the log two-bar chart).
#'
#' @param stats list from `compute_manuscript_stats`.
#' @param figure_dir output directory.
#' @return named character vector of figure basenames.
build_manuscript_figures <- function(stats, figure_dir) {
  ensure_dir(figure_dir)
  f <- function(n) file.path(figure_dir, n)

  # Figure 1: node count by depth (log scale, labeled).
  dc <- stats$depth_counts
  dc$label <- c("Depth 0\nseeds", "Depth 1\ndirect citers",
                "Depth 2\nterminal nodes")[match(dc$depth, c(0, 1, 2))]
  p1 <- ggplot2::ggplot(dc, ggplot2::aes(stats::reorder(label, depth), node_count)) +
    ggplot2::geom_col(fill = "#3b82f6", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = ms_comma(node_count)),
                       vjust = -0.4, size = 3.6) +
    ggplot2::scale_y_log10(labels = scales::comma,
                           expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Nodes (log scale)",
                  title = "Depth-2 network size by frontier layer") +
    MS_THEME
  manuscript_save(p1, f("manuscript_fig1_depth_counts"), 7, 4.5)

  # Figure 2: reach survival (CCDF) + Lorenz concentration (replaces hairball).
  r <- stats$reach_vector
  n <- length(r)
  pos <- sort(r[r > 0])
  ccdf <- tibble::tibble(
    reach = pos,
    surv = vapply(pos, function(x) mean(r >= x), numeric(1))
  ) |> dplyr::distinct(reach, .keep_all = TRUE)
  ymin <- min(ccdf$surv)
  ann <- tibble::tibble(
    x = c(stats$reach$reach_median, stats$reach$reach_p99, stats$reach$reach_max),
    y = ymin,
    lab = c(paste0("median ", ms_comma(stats$reach$reach_median)),
            paste0("p99 ", ms_comma(round(stats$reach$reach_p99))),
            paste0("max ", ms_comma(stats$reach$reach_max)))
  )
  p2a <- ggplot2::ggplot(ccdf, ggplot2::aes(reach, surv)) +
    ggplot2::geom_step(color = "#b91c1c", linewidth = 0.8) +
    ggplot2::geom_vline(data = ann, ggplot2::aes(xintercept = x),
                        linetype = "dashed", color = "grey55") +
    ggplot2::geom_text(data = ann, ggplot2::aes(x = x, y = y, label = lab),
                       angle = 90, vjust = -0.3, hjust = 0, size = 3,
                       color = "grey35") +
    ggplot2::scale_x_log10(labels = scales::comma) +
    ggplot2::scale_y_log10(labels = scales::percent) +
    ggplot2::labs(
      x = "Unique depth-2 reach per seed (log)",
      y = "Share of seeds with at least this reach (log)",
      title = "A. Downstream reach is heavy-tailed",
      subtitle = paste0(ms_comma(stats$reach$zero_reach_n), " of ",
                        ms_comma(stats$reach$seed_papers), " seeds (",
                        round(stats$reach$zero_reach_pct), "%) have zero reach")) +
    MS_THEME
  ls <- sort(r)
  lor <- tibble::tibble(
    p = seq_len(n) / n,
    L = cumsum(ls) / sum(ls)
  )
  lor <- lor[unique(round(seq(1, n, length.out = min(n, 2000)))), ]
  p2b <- ggplot2::ggplot(lor, ggplot2::aes(p, L)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "grey65") +
    ggplot2::geom_line(color = "#b91c1c", linewidth = 0.9) +
    ggplot2::annotate("text", x = 0.05, y = 0.9, hjust = 0, size = 3.4,
                      label = paste0("Gini = ",
                                     formatC(stats$reach$reach_gini, digits = 3,
                                             format = "f")),
                      color = "grey25") +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      x = "Cumulative share of seed papers (least to most reach)",
      y = "Cumulative share of total depth-2 reach",
      title = "B. Reach is highly concentrated") +
    MS_THEME
  p2 <- patchwork::wrap_plots(p2a, p2b, ncol = 2) +
    patchwork::plot_annotation(
      title = "Seed-level downstream reach: heavy-tailed and concentrated")
  manuscript_save(p2, f("manuscript_fig2_reach_survival_concentration"), 11, 5)

  # Figure 3: edge counts by depth.
  ed <- stats$edge_depths |>
    dplyr::group_by(depth) |>
    dplyr::summarise(edge_count = sum(edge_count), .groups = "drop")
  ed$label <- c("Depth 1\nseed <- direct citer",
                "Depth 2\ndirect citer <- descendant")[match(ed$depth, c(1, 2))]
  p3 <- ggplot2::ggplot(ed, ggplot2::aes(stats::reorder(label, depth), edge_count)) +
    ggplot2::geom_col(fill = "#6366f1", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = ms_comma(edge_count)),
                       vjust = -0.4, size = 3.8) +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Citation edges",
                  title = "Citation edges by graph layer") +
    MS_THEME
  manuscript_save(p3, f("manuscript_fig3_edge_depths"), 7, 4.5)

  # Figure 4: reach distribution histogram (log1p x).
  rd <- tibble::tibble(reach = r)
  p4 <- ggplot2::ggplot(rd, ggplot2::aes(reach + 1)) +
    ggplot2::geom_histogram(bins = 60, fill = "#0ea5e9", color = "white",
                            linewidth = 0.1) +
    ggplot2::scale_x_log10(labels = scales::comma) +
    ggplot2::labs(
      x = "Total unique depth-2 reach per seed, +1 (log scale)",
      y = "Number of seed papers",
      title = "Distribution of seed-level downstream reach",
      subtitle = paste0("Median ", ms_comma(stats$reach$reach_median),
                        "; mean ", formatC(stats$reach$reach_mean, digits = 1,
                                           format = "f"),
                        "; max ", ms_comma(stats$reach$reach_max))) +
    MS_THEME
  manuscript_save(p4, f("manuscript_fig4_reach_distribution"), 8, 4.5)

  # Figure 5: top-seed neighborhoods, stacked direct vs depth-2.
  t10 <- stats$top10 |>
    dplyr::mutate(short = substr(ifelse(is.na(title), record_id, title), 1, 55),
                  rank = dplyr::row_number(),
                  short = sprintf("%2d. %s", rank, short))
  t10l <- t10 |>
    dplyr::select(short, rank, direct_citers, depth2_descendants) |>
    tidyr::pivot_longer(c(direct_citers, depth2_descendants),
                        names_to = "layer", values_to = "count") |>
    dplyr::mutate(layer = factor(
      ifelse(layer == "direct_citers", "Direct citers", "Depth-2 descendants"),
      levels = c("Depth-2 descendants", "Direct citers")))
  p5 <- ggplot2::ggplot(t10l, ggplot2::aes(stats::reorder(short, -rank), count,
                                           fill = layer)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_fill_manual(values = c("Depth-2 descendants" = "#1d4ed8",
                                          "Direct citers" = "#f97316")) +
    ggplot2::labs(x = NULL, y = "Unique nodes", fill = NULL,
                  title = "Largest seed-level citation neighborhoods") +
    MS_THEME + ggplot2::theme(legend.position = "top")
  manuscript_save(p5, f("manuscript_fig5_top_seed_reach"), 10, 6)

  # Figure 6: post-notice event study (replaces the log two-bar chart).
  ev <- stats$event |>
    dplyr::filter(!is.na(offset_year), offset_year >= -15, offset_year <= 15) |>
    dplyr::mutate(period = ifelse(offset_year > 0, "After notice",
                                  "Before / same year"))
  p6 <- ggplot2::ggplot(ev, ggplot2::aes(offset_year, n, fill = period)) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey40") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_fill_manual(values = c("Before / same year" = "#94a3b8",
                                          "After notice" = "#b91c1c")) +
    ggplot2::labs(
      x = "Publication year of citing work, relative to notice year",
      y = "Direct citations of seed papers", fill = NULL,
      title = "Direct citations of retracted/concerning papers around the notice year",
      subtitle = paste0(
        ms_comma(stats$post_notice$direct_unique_nodes),
        " unique papers cite a seed after its notice (",
        ms_comma(stats$post_notice$seeds_with_post_direct), " of ",
        ms_comma(stats$reach$seed_papers), " seeds affected)")) +
    MS_THEME + ggplot2::theme(legend.position = "top")
  manuscript_save(p6, f("manuscript_fig6_post_notice_eventstudy"), 9, 4.8)

  c(fig1 = "manuscript_fig1_depth_counts",
    fig2 = "manuscript_fig2_reach_survival_concentration",
    fig3 = "manuscript_fig3_edge_depths",
    fig4 = "manuscript_fig4_reach_distribution",
    fig5 = "manuscript_fig5_top_seed_reach",
    fig6 = "manuscript_fig6_post_notice_eventstudy")
}

#' Build the reproducible manuscript: stats JSON, figures, and rendered docs.
#'
#' @param store StudyStore.
#' @param output_dir analysis output root (`outputs/opencitations`).
#' @param max_analysis_depth integer.
#' @param render logical; render the Quarto manuscript when Quarto is available.
#' @param compute_unique_post_d2 logical; compute the unique post-notice depth-2
#'   node count (a full two-hop scan).
#' @return path to the stats JSON.
#' @export
build_manuscript <- function(store, output_dir, max_analysis_depth = 2L,
                             render = TRUE, compute_unique_post_d2 = TRUE) {
  ensure_dir(output_dir)
  table_dir <- file.path(output_dir, "tables")
  figure_dir <- file.path(output_dir, "figures")

  stats <- compute_manuscript_stats(store, table_dir, max_analysis_depth,
                                    compute_unique_post_d2)
  figs <- build_manuscript_figures(stats, figure_dir)

  stats_out <- stats
  stats_out$reach_vector <- NULL
  stats_out$direct_vector <- NULL
  stats_out$depth2_vector <- NULL
  stats_json <- file.path(output_dir, "manuscript_stats.json")
  jsonlite::write_json(stats_out, stats_json, auto_unbox = TRUE, pretty = TRUE,
                       dataframe = "rows", na = "null")

  if (isTRUE(render)) {
    rendered <- tryCatch(
      render_manuscript(output_dir, stats_json),
      error = function(e) {
        message("Manuscript render skipped: ", conditionMessage(e))
        NULL
      })
    if (!is.null(rendered)) message("Manuscript rendered: ", rendered)
  }
  invisible(stats_json)
}

#' Render the bundled manuscript Quarto template to HTML/PDF/DOCX/GFM.
#' @param output_dir analysis output root.
#' @param stats_json path to the computed stats JSON.
#' @return path to the output directory (invisibly), or NULL if Quarto absent.
render_manuscript <- function(output_dir, stats_json) {
  template <- system.file("manuscript", "manuscript.qmd",
                          package = "retractionpollution")
  if (!nzchar(template) || !file.exists(template)) {
    local <- file.path("inst", "manuscript", "manuscript.qmd")
    if (file.exists(local)) template <- local
  }
  if (!nzchar(template) || !file.exists(template)) {
    stop("manuscript.qmd template not found")
  }
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("quarto R package not available")
  }
  target <- file.path(output_dir, "manuscript.qmd")
  file.copy(template, target, overwrite = TRUE)
  params <- list(stats_json = normalizePath(stats_json))
  for (fmt in c("gfm", "html", "pdf", "docx")) {
    tryCatch(
      quarto::quarto_render(input = target, output_format = fmt,
                            execute_params = params, quiet = TRUE),
      error = function(e) message("  format ", fmt, " failed: ",
                                  conditionMessage(e)))
  }
  # Quarto writes manuscript.md for gfm; normalize the name.
  gfm_out <- file.path(output_dir, "manuscript.md")
  if (!file.exists(gfm_out)) {
    alt <- file.path(output_dir, "manuscript.gfm")
    if (file.exists(alt)) file.rename(alt, gfm_out)
  }
  invisible(output_dir)
}
