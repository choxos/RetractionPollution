#' @title Matched non-retracted control comparison
#' @description Builds a matched-control arm for the post-notice citation
#'   analysis (audit finding M1). Each retracted or concerning seed paper is
#'   matched to a non-retracted control paper of the same publication year and
#'   similar citation in-degree, drawn from the depth-1 layer (papers with a
#'   complete, dated incoming-citation history in the study database). The
#'   control is assigned the matched seed's notice date as a pseudo-event, and
#'   the share of each paper's direct citations that fall after that date is
#'   compared within pairs.
#'
#'   This is a descriptive matched comparison, not a randomized experiment: the
#'   control pool consists of non-retracted papers that themselves cite a
#'   retracted seed (the only papers in the OpenCitations-only graph with a
#'   fully crawled incoming-citation history), which makes the test conservative
#'   because controls are drawn from the same citation neighborhoods.

library(dplyr)
library(ggplot2)

#' Nearest-neighbor match on (exact year, log in-degree) with replacement.
#'
#' Matching is stratified by exact publication year; within a year the nearest
#' control on `log1p(indeg)` is selected. With-replacement matching is used
#' because the control pool is far larger than the seed set; the number of
#' unique controls used is reported.
#'
#' @param seeds data.frame with `openalex_id`, `year`, `indeg`, `logdeg`.
#' @param pool data.frame with `openalex_id`, `year`, `indeg`, `logdeg`.
#' @param jitter numeric tie-break jitter added to seed targets (deterministic).
#' @return `seeds` with `control_id` and `control_indeg` columns (unmatched
#'   seeds dropped).
#' @noRd
match_controls_nn <- function(seeds, pool, jitter = 0) {
  out <- vector("list", length(unique(seeds$year)))
  i <- 0L
  for (yr in sort(unique(seeds$year))) {
    s <- seeds[seeds$year == yr, , drop = FALSE]
    p <- pool[pool$year == yr, , drop = FALSE]
    if (nrow(p) == 0L || nrow(s) == 0L) next
    # Stable secondary sort on id so ties break deterministically across runs
    # (DuckDB row order for equal in-degrees is not guaranteed).
    ord <- order(p$logdeg, p$openalex_id)
    p <- p[ord, , drop = FALSE]
    targets <- s$logdeg + jitter
    pos <- findInterval(targets, p$logdeg, all.inside = TRUE)
    # Compare neighbor at pos and pos+1, choose nearer.
    lo <- pmax(pos, 1L)
    hi <- pmin(pos + 1L, nrow(p))
    d_lo <- abs(targets - p$logdeg[lo])
    d_hi <- abs(targets - p$logdeg[hi])
    pick <- ifelse(d_lo <= d_hi, lo, hi)
    s$control_id <- p$openalex_id[pick]
    s$control_indeg <- p$indeg[pick]
    i <- i + 1L
    out[[i]] <- s
  }
  dplyr::bind_rows(out)
}

#' Build the matched-control comparison, writing tables and a figure.
#'
#' @param store StudyStore.
#' @param output_dir analysis output root.
#' @return named list of control-comparison statistics (also embedded in the
#'   manuscript stats).
#' @export
build_control_comparison <- function(store, output_dir) {
  con <- store$con
  table_dir <- ensure_dir(file.path(output_dir, "tables"))
  figure_dir <- ensure_dir(file.path(output_dir, "figures"))

  # --- Seed frame: dated direct citers + post-notice count per resolved seed --
  seeds <- DBI::dbGetQuery(con, "
    WITH canon AS (
      SELECT openalex_id, MIN(notice_date) AS nd
      FROM seeds WHERE resolved_status = 'resolved' AND openalex_id IS NOT NULL
      GROUP BY openalex_id
    )
    SELECT c.openalex_id, c.nd AS notice_date, w.publication_year AS year,
           COUNT(*) AS indeg,
           SUM(CASE WHEN cw.publication_date > c.nd THEN 1 ELSE 0 END) AS post_in
    FROM canon c
    JOIN works w ON c.openalex_id = w.openalex_id
    JOIN citation_edges e ON e.target_id = c.openalex_id AND e.depth = 1
    JOIN works cw ON e.source_id = cw.openalex_id
    WHERE w.publication_year IS NOT NULL AND cw.publication_date IS NOT NULL
    GROUP BY c.openalex_id, c.nd, w.publication_year")
  seeds <- seeds[seeds$indeg > 0L, , drop = FALSE]
  seeds$seed_share <- seeds$post_in / seeds$indeg
  seeds$logdeg <- log1p(seeds$indeg)

  # --- Control pool: depth-1 non-retracted papers, dated in-degree -----------
  pool <- DBI::dbGetQuery(con, "
    SELECT ft.openalex_id, w.publication_year AS year, COUNT(*) AS indeg
    FROM citation_edges e
    JOIN frontier_nodes ft ON e.target_id = ft.openalex_id AND ft.depth = 1
    JOIN works w ON ft.openalex_id = w.openalex_id
    JOIN works cw ON e.source_id = cw.openalex_id
    WHERE e.depth = 2 AND w.publication_year IS NOT NULL
      AND (w.is_retracted IS NULL OR w.is_retracted = FALSE)
      AND cw.publication_date IS NOT NULL
    GROUP BY ft.openalex_id, w.publication_year")
  pool$logdeg <- log1p(pool$indeg)

  matched <- match_controls_nn(seeds, pool)
  if (nrow(matched) == 0L) {
    return(list(matched_pairs = 0L))
  }
  matched$pair_id <- seq_len(nrow(matched))

  # --- Control post-notice share, using the matched seed's notice date -------
  pairs_tbl <- paste0("tmp_ctrl_pairs_", Sys.getpid())
  DBI::dbWriteTable(
    con, pairs_tbl,
    data.frame(pair_id = matched$pair_id, control_id = matched$control_id,
               notice_date = matched$notice_date,
               notice_year = as.integer(format(matched$notice_date, "%Y")),
               stringsAsFactors = FALSE),
    temporary = TRUE, overwrite = TRUE)
  ref <- DBI::dbQuoteIdentifier(con, pairs_tbl)

  ctrl <- DBI::dbGetQuery(con, sprintf("
    SELECT m.pair_id,
           COUNT(*) AS control_indeg_dated,
           SUM(CASE WHEN cw.publication_date > m.notice_date THEN 1 ELSE 0 END)
             AS control_post_in
    FROM %s m
    JOIN citation_edges e ON e.target_id = m.control_id AND e.depth = 2
    JOIN works cw ON e.source_id = cw.openalex_id
    WHERE cw.publication_date IS NOT NULL
    GROUP BY m.pair_id", ref))

  paired <- matched |>
    dplyr::left_join(ctrl, by = "pair_id") |>
    dplyr::mutate(
      control_post_in = tidyr::replace_na(control_post_in, 0L),
      control_indeg_dated = tidyr::replace_na(control_indeg_dated, 0L),
      control_share = ifelse(control_indeg_dated > 0,
                             control_post_in / control_indeg_dated, NA_real_)
    ) |>
    dplyr::filter(!is.na(control_share))

  readr::write_csv(
    paired |>
      dplyr::transmute(pair_id, year, seed_id = openalex_id, control_id,
                       seed_indeg = indeg, control_indeg = control_indeg_dated,
                       seed_share, control_share,
                       share_diff = seed_share - control_share),
    file.path(table_dir, "control_matched_pairs.csv"))

  # --- Paired comparison statistics ------------------------------------------
  wt <- suppressWarnings(stats::wilcox.test(
    paired$seed_share, paired$control_share, paired = TRUE))
  diff <- paired$seed_share - paired$control_share
  stat <- list(
    matched_pairs = nrow(paired),
    unique_controls = dplyr::n_distinct(paired$control_id),
    seed_share_median = stats::median(paired$seed_share),
    control_share_median = stats::median(paired$control_share),
    median_share_diff = stats::median(diff),
    mean_share_diff = mean(diff),
    pct_pairs_seed_higher = 100 * mean(diff > 0),
    wilcoxon_p = wt$p.value,
    seed_share_mean = mean(paired$seed_share),
    control_share_mean = mean(paired$control_share)
  )
  readr::write_csv(
    tibble::tibble(metric = names(stat),
                   value = as.character(unlist(stat, use.names = FALSE))),
    file.path(table_dir, "control_summary.csv"))

  # --- Event-study overlay: citations by year relative to notice -------------
  seeds_tbl <- paste0("tmp_ctrl_seeds_", Sys.getpid())
  DBI::dbWriteTable(
    con, seeds_tbl,
    data.frame(seed_id = matched$openalex_id[matched$pair_id %in% paired$pair_id],
               notice_year = as.integer(format(
                 matched$notice_date[matched$pair_id %in% paired$pair_id], "%Y")),
               stringsAsFactors = FALSE),
    temporary = TRUE, overwrite = TRUE)
  sref <- DBI::dbQuoteIdentifier(con, seeds_tbl)

  seed_ev <- DBI::dbGetQuery(con, sprintf("
    SELECT (CAST(cw.publication_year AS INTEGER) - m.notice_year) AS offset_year,
           COUNT(*) AS n
    FROM %s m
    JOIN citation_edges e ON e.target_id = m.seed_id AND e.depth = 1
    JOIN works cw ON e.source_id = cw.openalex_id
    WHERE cw.publication_year IS NOT NULL
    GROUP BY offset_year", sref))
  seed_ev$group <- "Retracted / concerning seeds"

  ctrl_ev <- DBI::dbGetQuery(con, sprintf("
    SELECT (CAST(cw.publication_year AS INTEGER) - m.notice_year) AS offset_year,
           COUNT(*) AS n
    FROM %s m
    JOIN citation_edges e ON e.target_id = m.control_id AND e.depth = 2
    JOIN works cw ON e.source_id = cw.openalex_id
    WHERE cw.publication_year IS NOT NULL
    GROUP BY offset_year", ref))
  ctrl_ev$group <- "Matched non-retracted controls"

  ev <- dplyr::bind_rows(seed_ev, ctrl_ev) |>
    dplyr::filter(!is.na(offset_year), offset_year >= -15, offset_year <= 15) |>
    dplyr::group_by(group) |>
    dplyr::mutate(share = n / sum(n)) |>
    dplyr::ungroup()

  build_control_figure(paired, ev, stat, figure_dir)

  DBI::dbRemoveTable(con, pairs_tbl)
  DBI::dbRemoveTable(con, seeds_tbl)
  stat
}

#' Two-panel control-comparison figure.
#' @noRd
build_control_figure <- function(paired, ev, stat, figure_dir) {
  p_ev <- ggplot2::ggplot(ev, ggplot2::aes(offset_year, share, color = group)) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed",
                        color = "grey45") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_color_manual(values = c(
      "Retracted / concerning seeds" = "#b91c1c",
      "Matched non-retracted controls" = "#334155")) +
    ggplot2::labs(
      x = "Citing-work year relative to notice year", y = "Share of citations",
      color = NULL, title = "A. Citation timing: seeds vs matched controls") +
    MS_THEME + ggplot2::theme(legend.position = "top")

  diffs <- tibble::tibble(share_diff = paired$seed_share - paired$control_share)
  p_hist <- ggplot2::ggplot(diffs, ggplot2::aes(share_diff)) +
    ggplot2::geom_histogram(bins = 40, fill = "#b91c1c", color = "white",
                            linewidth = 0.1) +
    ggplot2::geom_vline(xintercept = 0, color = "grey45") +
    ggplot2::geom_vline(xintercept = stat$median_share_diff,
                        color = "#1d4ed8", linewidth = 0.8) +
    ggplot2::labs(
      x = "Post-notice citation share: seed minus matched control",
      y = "Matched pairs",
      title = "B. Paired difference in post-notice citation share",
      subtitle = sprintf(
        "median seed %.1f%% vs control %.1f%%; seed higher in %.0f%% of pairs",
        100 * stat$seed_share_median, 100 * stat$control_share_median,
        stat$pct_pairs_seed_higher)) +
    MS_THEME

  fig <- patchwork::wrap_plots(p_ev, p_hist, ncol = 2) +
    patchwork::plot_annotation(
      title = "Matched control comparison of post-notice citation share")
  manuscript_save(fig, file.path(figure_dir,
                                 "manuscript_fig7_control_comparison"), 12, 5)
}
