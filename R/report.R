#' @title Report module for retractionpollution
#' @description Produces a narrative study report (MED-6 fix). The original
#'   Python `report.py` was thin boilerplate that listed a few metrics and the
#'   crawl log. This port generates a real narrative report with provenance,
#'   summary metrics, per-source methodology, figures, top-seed tables,
#'   post-notice timeline, collection notes, and crawl metadata, written both
#'   as a plain `report.md` fallback and (when Quarto is installed) a rendered
#'   `report.html` from the bundled `report.qmd` template.

library(readr)

#' Write a narrative study report.
#'
#' Reads analysis artifacts (`tables/summary.csv`,
#' `tables/top_polluted_seeds.csv`, `tables/depth_counts.csv`) and crawl
#' provenance metadata from the store, copies the bundled Quarto template
#' into `output_dir`, renders it to HTML via `quarto::quarto_render` (if
#' available), and always writes a plain `report.md` fallback that carries
#' the same narrative content.
#'
#' @param store `StudyStore` instance.
#' @param output_dir character path to the analysis output root.
#' @return path to `report.md` (the always-present narrative fallback).
#' @export
write_report <- function(store, output_dir) {
  ensure_dir(output_dir)
  table_dir <- file.path(output_dir, "tables")
  figure_dir <- file.path(output_dir, "figures")

  summary_list <- read_summary_list(file.path(table_dir, "summary.csv"))
  crawl_summary <- store$get_metadata("last_crawl_summary", "{}")
  depth3_truncated <- store$get_metadata("depth3_truncated", "false")
  rw_snapshot_date <- store$get_metadata("rw_snapshot_date", "unknown")
  oc_access_date <- store$get_metadata("oc_access_date", "unknown")
  openalex_access_date <- store$get_metadata("openalex_access_date", "unknown")
  pipeline_mode <- store$get_metadata("pipeline_mode", "unknown")

  top_seeds <- read_csv_safe(file.path(table_dir, "top_polluted_seeds.csv"))
  top_seeds_10 <- if (!is.null(top_seeds) && nrow(top_seeds) > 0L) {
    utils::head(top_seeds, 10L)
  } else {
    data.frame()
  }
  depth_counts <- read_csv_safe(file.path(table_dir, "depth_counts.csv"))

  summary_json <- if (length(summary_list) > 0L) {
    jsonlite::toJSON(as.list(summary_list), auto_unbox = TRUE, pretty = TRUE)
  } else ""

  report_md_path <- file.path(output_dir, "report.md")
  if (!file.exists(report_md_path)) {
    writeLines("# Retraction Pollution Study Report", report_md_path)
  }

  # Persist one final validation table and embed the same table in both report
  # formats. The placeholder above lets report_exists validate before rendering.
  validation <- validate_study_outputs(store, output_dir,
                                        include_report = TRUE)

  report_md <- write_markdown_report(
    path = report_md_path,
    summary_list = summary_list,
    crawl_summary = crawl_summary,
    depth3_truncated = depth3_truncated,
    rw_snapshot_date = rw_snapshot_date,
    oc_access_date = oc_access_date,
    openalex_access_date = openalex_access_date,
    pipeline_mode = pipeline_mode,
    top_seeds = top_seeds_10,
    depth_counts = depth_counts,
    validation = validation,
    figure_dir = figure_dir
  )

  # --- Quarto render (best effort) -----------------------------------------
  quarto_path <- tryCatch(render_quarto(
    output_dir = output_dir,
    summary_json = as.character(summary_json),
    crawl_summary = crawl_summary,
    depth3_truncated = depth3_truncated,
    rw_snapshot_date = rw_snapshot_date,
    oc_access_date = oc_access_date,
    openalex_access_date = openalex_access_date,
    pipeline_mode = pipeline_mode,
    top_seeds = top_seeds_10,
    depth_counts = if (is.null(depth_counts)) data.frame() else depth_counts,
    validation = validation
  ), error = function(e) {
    # Quarto not available or render failed; the markdown fallback still ships.
    NULL
  })

  # Prefer HTML if Quarto actually produced it, else the markdown.
  if (!is.null(quarto_path) && file.exists(quarto_path)) quarto_path else report_md
}

#' Read `summary.csv` (columns: metric, value) into a named list.
#' @param path character.
#' @return named list of strings; empty list if file missing.
read_summary_list <- function(path) {
  if (!file.exists(path)) return(list())
  df <- tryCatch(
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      na = character()
    ),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0L) return(list())
  if (!all(c("metric", "value") %in% names(df))) return(list())
  vals <- as.character(df$value)
  stats::setNames(vals, df$metric)
}

#' Read a CSV safely, returning NULL on missing/error.
#' @param path character.
#' @return data.frame or NULL.
read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) NULL
  )
}

#' Copy the bundled Quarto template and render it.
#'
#' @param output_dir character.
#' @param ... named params forwarded to `quarto::quarto_render(execute_params=)`.
#' @return path to the rendered HTML, or NULL.
#' @noRd
render_quarto <- function(output_dir, summary_json, crawl_summary,
                          depth3_truncated, rw_snapshot_date,
                          oc_access_date, openalex_access_date,
                          pipeline_mode, top_seeds, depth_counts,
                          validation) {
  template <- system.file("report", "report.qmd", package = "retractionpollution")
  if (!nzchar(template) || !file.exists(template)) {
    local_template <- file.path("inst", "report", "report.qmd")
    if (file.exists(local_template)) template <- local_template
  }
  if (!nzchar(template) || !file.exists(template)) return(NULL)
  if (!requireNamespace("quarto", quietly = TRUE)) return(NULL)

  target <- file.path(output_dir, "report.qmd")
  file.copy(template, target, overwrite = TRUE)

  params <- list(
    summary = summary_json,
    crawl_summary = crawl_summary,
    depth3_truncated = depth3_truncated,
    rw_snapshot_date = rw_snapshot_date,
    oc_access_date = oc_access_date,
    openalex_access_date = openalex_access_date,
    pipeline_mode = pipeline_mode,
    top_seeds = top_seeds,
    depth_counts = depth_counts,
    validation = validation
  )

  quarto::quarto_render(
    input = target,
    output_file = "report.html",
    execute_dir = output_dir,
    execute_params = params
  )
  html_path <- file.path(output_dir, "report.html")
  if (file.exists(html_path)) html_path else NULL
}

#' Write the plain markdown narrative report.
#'
#' @param path character.
#' @param summary_list named list.
#' @param crawl_summary character JSON string.
#' @param depth3_truncated character.
#' @param rw_snapshot_date character.
#' @param oc_access_date character.
#' @param openalex_access_date character.
#' @param top_seeds data.frame (top 10).
#' @param depth_counts data.frame or NULL.
#' @param figure_dir character.
#' @return path invisibly.
#' @noRd
write_markdown_report <- function(path, summary_list, crawl_summary,
                                  depth3_truncated, rw_snapshot_date,
                                  oc_access_date, openalex_access_date,
                                  pipeline_mode, top_seeds, depth_counts,
                                  validation, figure_dir) {
  lines <- c(
    "# Retraction Pollution Study Report",
    "",
    paste0("_Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "_"),
    "",
    "## Provenance",
    "",
    paste0("- **Retraction Watch** (Retraction + Expression of Concern): ",
           rw_snapshot_date),
    paste0("- **OpenCitations** (DOI-based citation index): ", oc_access_date),
    paste0("- **OpenAlex** (work resolution + recursive expansion): ",
           openalex_access_date),
    "",
    "## Summary Metrics",
    ""
  )

  if (length(summary_list) > 0L) {
    for (metric in names(summary_list)) {
      lines <- c(lines, paste0("- **", metric, "**: ", summary_list[[metric]]))
    }
  } else {
    lines <- c(lines, "- No analysis summary found. Run `run_analysis()` first.")
  }

  openalex_note <- if (identical(pipeline_mode, "opencitations") ||
                       identical(summary_list[["openalex_edges"]], "0")) {
    "- **OpenAlex.** Not used for the inspected OpenCitations-only output; OpenAlex edge count is 0."
  } else {
    paste(
      "- **OpenAlex.** Used to resolve seeds and frontier nodes to canonical",
      "work records, and to perform recursive referenced-works expansion when",
      "OpenCitations returns no edges. Edges are tagged `source_api = openalex`",
      "when sourced there."
    )
  }

  lines <- c(lines, "", "## Data Sources", "",
    "- **Retraction Watch.** Seed records are drawn from the Retraction Watch",
    "  database, filtered to `Retraction` and `Expression of Concern` notice",
    "  types. Each seed carries the original DOI, notice date, and (where",
    "  available) the OpenAlex ID of the retracted/concerning paper.",
    "- **OpenCitations.** DOI-to-DOI citation edges are first sourced from",
    "  OpenCitations (COCI) for every parent node that carries a DOI, at each",
    "  frontier depth. This gives the most complete coverage of",
    "  non-OpenAlex-indexed citations.",
    openalex_note,
    "")

  # Network size by depth.
  depth_png <- file.path(figure_dir, "depth_counts.png")
  lines <- c(lines, "## Network Size by Depth", "")
  incomplete_crawl <- is.data.frame(validation) &&
    any(validation$check == "frontier_processed_for_depth2_claim" &
          validation$status == "fail")
  if (isTRUE(incomplete_crawl)) {
    lines <- c(lines,
      "**Incomplete crawl warning:** depth-2 graph counts are lower-bound",
      "counts from the processed frontier, not complete depth-2 census values.",
      "")
  }
  if (file.exists(depth_png)) {
    lines <- c(lines, "![Network size by depth](figures/depth_counts.png)", "")
  } else {
    lines <- c(lines, "_No depth-counts figure available._", "")
  }
  if (!is.null(depth_counts) && nrow(depth_counts) > 0L) {
    lines <- c(lines, knitr_table_md(depth_counts), "")
  }

  # Most polluted seeds.
  top_png <- file.path(figure_dir, "top_polluted_seeds.png")
  lines <- c(lines, "## Most Polluted Seeds", "")
  if (is.data.frame(top_seeds) && nrow(top_seeds) > 0L) {
    show_cols <- intersect(
      c("record_id", "title", "notice_type", "direct_citers",
        "seed_record_count", "depth2_descendants", "total_depth2_reach",
        "post_notice_direct_citers", "post_notice_depth2_descendants"),
      names(top_seeds)
    )
    lines <- c(lines, knitr_table_md(top_seeds[, show_cols, drop = FALSE]), "")
  } else {
    lines <- c(lines, "_No seed metrics available._", "")
  }
  if (file.exists(top_png)) {
    lines <- c(lines, "![Most polluted seeds](figures/top_polluted_seeds.png)", "")
  }

  # Post-notice timeline.
  timeline_png <- file.path(figure_dir, "post_notice_timeline.png")
  lines <- c(lines, "## Post-Notice Citation Timeline", "")
  if (file.exists(timeline_png)) {
    lines <- c(lines, "![Post-notice timeline](figures/post_notice_timeline.png)",
               "",
               "Direct citations to seed papers whose publication date falls",
               "**after** the seed's retraction / EOC notice date. This is a",
               "descriptive count, not a citation-rate model or causal effect",
               "of the notice on later citation behavior.",
               "")
  } else {
    lines <- c(lines,
      "_No post-notice citations were found; either no seed has a parseable",
      "notice date, or no citing work was published after its target's",
      "notice date._",
      "")
  }

  lines <- c(lines, "## Artifact Validation", "")
  if (is.data.frame(validation) && nrow(validation) > 0L) {
    lines <- c(lines, knitr_table_md(validation), "")
  } else {
    lines <- c(lines, "_No artifact validation table available._", "")
  }

  # Collection notes.
  lines <- c(lines, "## Collection Notes", "",
    "- **Depth-2 scope.** The headline tables export the induced frontier graph",
    "  through depth 2 (seeds -> direct citers -> depth-2 descendants). The",
    "  artifact validation table records whether the crawl state supports a",
    "  complete-depth claim; when that check fails, depth-2 metrics are lower",
    "  bounds from the processed frontier.",
    "- **Depth-3 cap.** Depth-3 expansion is optional and capped to keep",
    "  runtime bounded.",
    paste0("- **Depth-3 truncation flag:** `", depth3_truncated, "`"),
    if (identical(pipeline_mode, "opencitations")) {
      paste("- **Collection mode.** This output was generated in",
            "OpenCitations-only mode; OpenAlex metadata enrichment and fallback",
            "expansion are not present.")
    } else {
      paste("- **OpenCitations-first ordering.** At each frontier depth,",
            "OpenCitations is queried first for DOI-bearing parent nodes;",
            "OpenAlex fills gaps and provides recursive expansion where",
            "OpenCitations returns nothing.")
    },
    "- **Min-depth edge merge.** Citation edges are stored with a min-depth",
    "  merge: when the same (source, target) edge is observed at multiple",
    "  depths, the shallowest depth wins. This preserves the earliest path",
    "  length at which a citation relationship was discovered.",
    "")

  # Crawl metadata.
  lines <- c(lines, "## Crawl Metadata", "", "```json", crawl_summary, "```", "")

  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

#' Render a data.frame as a pipe-table markdown string (no knitr dependency).
#' @param df data.frame.
#' @return character vector of lines.
knitr_table_md <- function(df) {
  if (nrow(df) == 0L) return("_(no rows)_")
  hdr <- paste(colnames(df), collapse = " | ")
  sep <- paste(rep("---", ncol(df)), collapse = " | ")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(df[i, , drop = FALSE], function(v) {
      if (is.na(v)) "" else as.character(v)
    }, character(1))
    paste(cells, collapse = " | ")
  }, character(1))
  c(hdr, sep, rows)
}
