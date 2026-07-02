#' @title Command-line interface for retractionpollution
#' @description Port of the original Python `retraction_pollution.cli`
#'   module. Exposes a `main()` entry point and one `cmd_*` helper per
#'   subcommand, mirroring the argparse layout of the Python CLI.

library(argparse)

#' Create an `OpenAlexClient` from settings.
#'
#' @param settings `Settings` instance.
#' @return An `OpenAlexClient` instance.
make_openalex <- function(settings) {
  if (is_missing(settings$openalex_api_key) ||
      is.na(settings$openalex_api_key) ||
      settings$openalex_api_key == "") {
    stop("OPENALEX_API_KEY is required for this workload. ",
         "Set it in your shell before running collection commands.",
         call. = FALSE)
  }
  OpenAlexClient$new(
    api_key = settings$openalex_api_key,
    email = settings$openalex_email,
    request_delay = settings$openalex_request_delay,
    rate_limit_sleep = settings$openalex_rate_limit_sleep
  )
}

#' Create an `OpenCitationsClient` from settings.
#'
#' @param settings `Settings` instance.
#' @return An `OpenCitationsClient` instance.
make_opencitations <- function(settings) {
  token <- settings$opencitations_token
  if (is_missing(token) || is.na(token) || token == "") token <- NULL
  OpenCitationsClient(token = token)
}

#' Resolve the Retraction Watch CSV path for a subcommand.
#'
#' @param args argparse namespace with a `csv` element.
#' @param settings `Settings` instance.
#' @return character path to the CSV.
resolve_csv_path <- function(args, settings) {
  if (!is_missing(args$csv) && !is.null(args$csv) && !is.na(args$csv) &&
      args$csv != "") {
    return(args$csv)
  }
  file.path(settings$raw_dir, "retraction_watch_latest.csv")
}

#' Download the Retraction Watch CSV.
#' @param args argparse namespace with `url`.
#' @param settings `Settings` instance.
#' @return integer exit code (0 on success).
cmd_fetch_rw <- function(args, settings) {
  url <- if (is_missing(args$url) || is.null(args$url) || is.na(args$url) ||
             args$url == "") {
    RETRACTION_WATCH_URL
  } else {
    args$url
  }
  path <- download_retraction_watch(settings$raw_dir, url = url)
  cat("Downloaded Retraction Watch CSV:", path, "\n")
  0L
}

#' Load Retraction Watch seeds and optionally resolve them via OpenAlex.
#' @param args argparse namespace.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_prepare_seeds <- function(args, settings) {
  csv_path <- resolve_csv_path(args, settings)
  if (!file.exists(csv_path)) {
    stop("Retraction Watch CSV not found: ", csv_path,
         ". Run `rpollute fetch-rw` first.", call. = FALSE)
  }
  seeds <- load_seed_rows(csv_path)
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  record_source_metadata(
    store,
    rw_path = csv_path,
    pipeline_mode = "hybrid",
    uses_opencitations = FALSE,
    uses_openalex = !isTRUE(args$no_resolve)
  )
  count <- store$upsert_seeds(seeds)
  cat("Loaded", count, "Retraction/Expression-of-concern seed records.\n")
  if (!isTRUE(args$no_resolve)) {
    client <- make_openalex(settings)
    limit <- if (is_missing(args$limit) || is.null(args$limit) ||
                 is.na(args$limit)) NA else as.integer(args$limit)
    stats <- resolve_pending_seeds(
      store, client,
      limit = limit,
      title_fallback = isTRUE(args$title_fallback)
    )
    cat("Resolved seeds with OpenAlex:", json_dumps(stats), "\n")
  }
  dup_count <- mark_duplicate_seed_resolutions(store)
  if (dup_count > 0L) {
    cat("Marked", dup_count, "duplicate DOI seed records as 'duplicate_doi'.\n")
  }
  store$export_parquet_tables(file.path(settings$processed_dir, "parquet"))
  0L
}

#' Resolve already-loaded seeds against OpenAlex.
#' @param args argparse namespace.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_resolve_seeds <- function(args, settings) {
  client <- make_openalex(settings)
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  limit <- if (is_missing(args$limit) || is.null(args$limit) ||
               is.na(args$limit)) NA else as.integer(args$limit)
  stats <- resolve_pending_seeds(
    store, client,
    limit = limit,
    title_fallback = isTRUE(args$title_fallback)
  )
  dup_count <- mark_duplicate_seed_resolutions(store)
  if (dup_count > 0L) {
    cat("Marked", dup_count, "duplicate DOI seed records as 'duplicate_doi'.\n")
  }
  store$export_parquet_tables(file.path(settings$processed_dir, "parquet"))
  cat("Resolved seeds with OpenAlex:", json_dumps(stats), "\n")
  0L
}

#' Show the current OpenAlex rate-limit budget.
#' @param args argparse namespace (unused).
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_rate_limit <- function(args, settings) {
  status <- make_openalex(settings)$rate_limit_status()
  cat(json_dumps(status), "\n")
  0L
}

#' Load Retraction Watch seeds as DOI nodes for an OpenCitations-only run.
#' @param args argparse namespace with `csv`.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_prepare_opencitations <- function(args, settings) {
  settings <- opencitations_only_settings(args, settings)
  csv_path <- resolve_csv_path(args, settings)
  if (!file.exists(csv_path)) {
    stop("Retraction Watch CSV not found: ", csv_path,
         ". Run `rpollute fetch-rw` first.", call. = FALSE)
  }
  seeds <- load_seed_rows(csv_path)
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  record_source_metadata(
    store,
    rw_path = csv_path,
    pipeline_mode = "opencitations",
    uses_opencitations = TRUE,
    uses_openalex = FALSE
  )
  stats <- prepare_opencitations_seeds(store, seeds)
  dup_count <- mark_duplicate_seed_resolutions(store)
  if (dup_count > 0L) {
    cat("Marked", dup_count, "duplicate DOI seed records as 'duplicate_doi'.\n")
  }
  store$export_parquet_tables(file.path(settings$processed_dir,
                                        "opencitations_parquet"))
  cat("Prepared OpenCitations-only seeds:", json_dumps(stats), "\n")
  cat("Database:", settings$db_path, "\n")
  0L
}

#' Run or resume the OpenCitations-only DOI citation crawl.
#' @param args argparse namespace.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_crawl_opencitations <- function(args, settings) {
  settings <- opencitations_only_settings(args, settings)
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  record_source_metadata(
    store,
    pipeline_mode = "opencitations",
    uses_opencitations = TRUE,
    uses_openalex = FALSE
  )
  crawler <- OpenCitationsOnlyCrawler$new(store, make_opencitations(settings))
  parent_limit <- if (is_missing(args$parent_limit) || is.null(args$parent_limit) ||
                      is.na(args$parent_limit)) NA else as.integer(args$parent_limit)
  summary <- crawler$crawl(
    max_depth = as.integer(args$max_depth),
    complete_depth = as.integer(args$complete_depth),
    depth3_node_cap = as.integer(args$depth3_node_cap),
    parent_limit = parent_limit
  )
  store$export_parquet_tables(file.path(settings$processed_dir,
                                        "opencitations_parquet"))
  cat("OpenCitations-only crawl summary:", json_dumps(summary), "\n")
  0L
}

#' Run or resume the hybrid OpenAlex + OpenCitations citation crawl.
#' @param args argparse namespace.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_crawl <- function(args, settings) {
  openalex <- make_openalex(settings)
  opencitations <- if (isTRUE(args$no_opencitations)) NULL else make_opencitations(settings)
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  record_source_metadata(
    store,
    pipeline_mode = "hybrid",
    uses_opencitations = !isTRUE(args$no_opencitations),
    uses_openalex = TRUE
  )
  crawler <- CitationCrawler$new(store, openalex, opencitations)
  summary <- crawler$crawl(
    max_depth = as.integer(args$max_depth),
    complete_depth = as.integer(args$complete_depth),
    batch_size = as.integer(args$batch_size),
    per_page = as.integer(args$per_page),
    depth3_node_cap = as.integer(args$depth3_node_cap),
    depth3_page_cap = as.integer(args$depth3_page_cap)
  )
  store$export_parquet_tables(file.path(settings$processed_dir, "parquet"))
  cat("Crawl summary:", json_dumps(summary), "\n")
  0L
}

#' Generate tables, figures, and graph exports.
#' @param args argparse namespace with `max_analysis_depth`.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_analyze <- function(args, settings) {
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  summary <- run_analysis(
    store, settings$output_dir,
    max_analysis_depth = as.integer(args$max_analysis_depth)
  )
  validation <- validate_study_outputs(
    store, settings$output_dir,
    max_analysis_depth = as.integer(args$max_analysis_depth),
    include_report = TRUE
  )
  cat("Analysis complete:", json_dumps(summary), "\n")
  cat("Artifact validation:", json_dumps(validation), "\n")
  0L
}

#' Write the report markdown.
#' @param args argparse namespace (unused).
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_report <- function(args, settings) {
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  path <- write_report(store, settings$output_dir)
  cat("Report written:", path, "\n")
  0L
}

#' Build the reproducible manuscript (stats, figures, rendered documents).
#' @param args argparse namespace with optional `no_render` / `no_unique_d2`.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_manuscript <- function(args, settings) {
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  path <- build_manuscript(
    store, settings$output_dir,
    max_analysis_depth = 2L,
    render = !isTRUE(args$no_render),
    compute_unique_post_d2 = !isTRUE(args$no_unique_d2)
  )
  cat("Manuscript stats written:", path, "\n")
  0L
}

#' Fetch, prepare, crawl, analyze, and report (OpenCitations-only).
#' @param args argparse namespace.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_run_opencitations <- function(args, settings) {
  settings <- opencitations_only_settings(args, settings)
  path <- download_retraction_watch(settings$raw_dir)
  cat("Downloaded Retraction Watch CSV:", path, "\n")
  seeds <- load_seed_rows(path)
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  record_source_metadata(
    store,
    rw_path = path,
    pipeline_mode = "opencitations",
    uses_opencitations = TRUE,
    uses_openalex = FALSE
  )
  seed_stats <- prepare_opencitations_seeds(store, seeds)
  dup_count <- mark_duplicate_seed_resolutions(store)
  if (dup_count > 0L) {
    cat("Marked", dup_count, "duplicate DOI seed records as 'duplicate_doi'.\n")
  }
  cat("Prepared OpenCitations-only seeds:", json_dumps(seed_stats), "\n")
  crawler <- OpenCitationsOnlyCrawler$new(store, make_opencitations(settings))
  parent_limit <- if (is_missing(args$parent_limit) || is.null(args$parent_limit) ||
                      is.na(args$parent_limit)) NA else as.integer(args$parent_limit)
  crawl_summary <- crawler$crawl(
    max_depth = as.integer(args$max_depth),
    complete_depth = as.integer(args$complete_depth),
    depth3_node_cap = as.integer(args$depth3_node_cap),
    parent_limit = parent_limit
  )
  cat("OpenCitations-only crawl summary:", json_dumps(crawl_summary), "\n")
  store$export_parquet_tables(file.path(settings$processed_dir,
                                        "opencitations_parquet"))
  analysis_summary <- run_analysis(store, settings$output_dir,
                                   max_analysis_depth = 2L)
  cat("Analysis complete:", json_dumps(analysis_summary), "\n")
  report_path <- write_report(store, settings$output_dir)
  cat("Report written:", report_path, "\n")
  manuscript_stats <- build_manuscript(store, settings$output_dir,
                                       max_analysis_depth = 2L)
  cat("Manuscript built:", manuscript_stats, "\n")
  0L
}

#' Fetch, prepare, crawl, analyze, and report (hybrid).
#' @param args argparse namespace.
#' @param settings `Settings` instance.
#' @return integer exit code.
cmd_run_all <- function(args, settings) {
  path <- download_retraction_watch(settings$raw_dir)
  cat("Downloaded Retraction Watch CSV:", path, "\n")
  seeds <- load_seed_rows(path)
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  record_source_metadata(
    store,
    rw_path = path,
    pipeline_mode = "hybrid",
    uses_opencitations = !isTRUE(args$no_opencitations),
    uses_openalex = TRUE
  )
  count <- store$upsert_seeds(seeds)
  cat("Loaded", count, "seed records.\n")
  stats <- resolve_pending_seeds(
    store, make_openalex(settings),
    title_fallback = isTRUE(args$title_fallback)
  )
  cat("Resolved seeds with OpenAlex:", json_dumps(stats), "\n")
  dup_count <- mark_duplicate_seed_resolutions(store)
  if (dup_count > 0L) {
    cat("Marked", dup_count, "duplicate DOI seed records as 'duplicate_doi'.\n")
  }
  crawler <- CitationCrawler$new(
    store,
    make_openalex(settings),
    if (isTRUE(args$no_opencitations)) NULL else make_opencitations(settings)
  )
  crawl_summary <- crawler$crawl(
    max_depth = as.integer(args$max_depth),
    complete_depth = as.integer(args$complete_depth),
    batch_size = as.integer(args$batch_size),
    per_page = as.integer(args$per_page),
    depth3_node_cap = as.integer(args$depth3_node_cap),
    depth3_page_cap = as.integer(args$depth3_page_cap)
  )
  cat("Crawl summary:", json_dumps(crawl_summary), "\n")
  store$export_parquet_tables(file.path(settings$processed_dir, "parquet"))
  analysis_summary <- run_analysis(store, settings$output_dir,
                                   max_analysis_depth = 2L)
  cat("Analysis complete:", json_dumps(analysis_summary), "\n")
  report_path <- write_report(store, settings$output_dir)
  cat("Report written:", report_path, "\n")
  manuscript_stats <- build_manuscript(store, settings$output_dir,
                                       max_analysis_depth = 2L)
  cat("Manuscript built:", manuscript_stats, "\n")
  0L
}

#' Fetch, prepare, crawl, analyze, and report from R.
#'
#' Thin wrapper around the same implementation used by the `run-all` CLI
#' subcommand. Intended for README examples and interactive use.
#'
#' @param data_dir character data directory.
#' @param output_dir character output directory.
#' @param db character DuckDB path, or `NA` for the default.
#' @param max_depth,complete_depth integer crawl depth bounds.
#' @param batch_size,per_page integer OpenAlex batching parameters.
#' @param depth3_node_cap,depth3_page_cap integer depth-3 caps.
#' @param no_opencitations logical; disable the OpenCitations supplement.
#' @param title_fallback logical; use OpenAlex title/author fallback search.
#' @return integer exit code.
#' @export
run_all <- function(data_dir = "data", output_dir = "outputs", db = NA,
                    max_depth = 3L, complete_depth = 2L,
                    batch_size = 100L, per_page = 100L,
                    depth3_node_cap = 250000L,
                    depth3_page_cap = 2500L,
                    no_opencitations = FALSE,
                    title_fallback = FALSE) {
  args <- list(
    max_depth = max_depth,
    complete_depth = complete_depth,
    batch_size = batch_size,
    per_page = per_page,
    depth3_node_cap = depth3_node_cap,
    depth3_page_cap = depth3_page_cap,
    no_opencitations = no_opencitations,
    title_fallback = title_fallback
  )
  settings <- Settings_from_env(data_dir, output_dir, db)
  cmd_run_all(args, settings)
}

#' Fetch, prepare, crawl, analyze, and report with OpenCitations only.
#'
#' Thin wrapper around the same implementation used by the
#' `run-opencitations` CLI subcommand. It does not require an OpenAlex key.
#'
#' @param data_dir character data directory.
#' @param output_dir character output directory.
#' @param db character DuckDB path, or `NA` for the default.
#' @param max_depth,complete_depth integer crawl depth bounds.
#' @param depth3_node_cap integer depth-3 node cap.
#' @param parent_limit integer or `NA`; cap on parents processed per run.
#' @return integer exit code.
#' @export
run_opencitations <- function(data_dir = "data", output_dir = "outputs",
                              db = NA, max_depth = 3L,
                              complete_depth = 2L,
                              depth3_node_cap = 250000L,
                              parent_limit = NA) {
  args <- list(
    db = db,
    output_dir = output_dir,
    max_depth = max_depth,
    complete_depth = complete_depth,
    depth3_node_cap = depth3_node_cap,
    parent_limit = parent_limit
  )
  settings <- Settings_from_env(data_dir, output_dir, db)
  cmd_run_opencitations(args, settings)
}

#' Build the reproducible manuscript from an existing study database.
#'
#' Computes every headline statistic and figure from the store and the analysis
#' tables, writes `manuscript_stats.json`, and renders the Quarto manuscript.
#'
#' @param data_dir character data directory.
#' @param output_dir character output directory.
#' @param db character DuckDB path, or `NA` for the default.
#' @param render logical; render the Quarto manuscript.
#' @param compute_unique_post_d2 logical; compute the unique post-notice depth-2
#'   node count (a full two-hop scan over all edges).
#' @return integer exit code.
#' @export
manuscript <- function(data_dir = "data", output_dir = "outputs", db = NA,
                       render = TRUE, compute_unique_post_d2 = TRUE) {
  settings <- Settings_from_env(data_dir, output_dir, db)
  store <- StudyStore$new(settings$db_path)
  on.exit(store$close(), add = TRUE)
  path <- build_manuscript(store, settings$output_dir, max_analysis_depth = 2L,
                           render = render,
                           compute_unique_post_d2 = compute_unique_post_d2)
  cat("Manuscript stats written:", path, "\n")
  0L
}

#' Dispatch table mapping subcommand names to `cmd_*` functions.
#'
#' The R `argparse` package cannot store closures via `set_defaults`, so we
#' resolve the subcommand handler from this table after parsing.
SUBCOMMAND_DISPATCH <- list(
  `fetch-rw` = cmd_fetch_rw,
  `prepare-seeds` = cmd_prepare_seeds,
  `resolve-seeds` = cmd_resolve_seeds,
  `rate-limit` = cmd_rate_limit,
  `prepare-opencitations` = cmd_prepare_opencitations,
  `prepare-oc` = cmd_prepare_opencitations,
  `crawl-opencitations` = cmd_crawl_opencitations,
  `crawl-oc` = cmd_crawl_opencitations,
  `crawl` = cmd_crawl,
  `analyze` = cmd_analyze,
  `report` = cmd_report,
  `manuscript` = cmd_manuscript,
  `run-all` = cmd_run_all,
  `run-opencitations` = cmd_run_opencitations,
  `run-oc` = cmd_run_opencitations
)

#' Build the argparse parser.
#'
#' Note: unlike the Python original, the R `argparse` package cannot attach
#' closures to the parsed namespace via `set_defaults`, so subcommand
#' handlers are resolved post-parse via `SUBCOMMAND_DISPATCH`.
#'
#' @return An `argparse::ArgumentParser` instance.
build_parser <- function() {
  parser <- argparse::ArgumentParser(
    prog = "rpollute",
    description = paste(
      "Trace citation pollution from Retraction Watch records through",
      "OpenCitations and OpenAlex."
    )
  )
  parser$add_argument("--data-dir", default = "data",
                      help = "Data directory, default: data")
  parser$add_argument("--output-dir", default = "outputs",
                      help = "Output directory, default: outputs")
  parser$add_argument("--db", type = "character", default = NULL,
                      help = "DuckDB path, default: data/processed/study.duckdb")

  subparsers <- parser$add_subparsers(dest = "command")

  fetch <- subparsers$add_parser("fetch-rw",
                                 help = "Download the Retraction Watch CSV")
  fetch$add_argument("--url", type = "character", default = NULL,
                     help = paste("Source URL (default: the Retraction Watch",
                                  "GitLab CSV export)"))

  prepare <- subparsers$add_parser("prepare-seeds",
                                   help = "Load and resolve Retraction Watch seeds")
  prepare$add_argument("--csv", type = "character", default = NULL,
                       help = "Retraction Watch CSV path")
  prepare$add_argument("--no-resolve", action = "store_true",
                       help = "Load seeds without OpenAlex resolution")
  prepare$add_argument("--limit", type = "integer", default = NULL,
                       help = "Limit seed resolution count")
  prepare$add_argument("--title-fallback", action = "store_true",
                        help = paste("Use expensive title/author OpenAlex search",
                                     "after DOI and PMID resolution fail"))

  resolve <- subparsers$add_parser(
    "resolve-seeds",
    help = "Resolve already loaded seed records to OpenAlex works"
  )
  resolve$add_argument("--limit", type = "integer", default = NULL,
                       help = "Limit seed resolution count")
  resolve$add_argument("--title-fallback", action = "store_true",
                        help = paste("Use expensive title/author OpenAlex search",
                                     "after DOI and PMID resolution fail"))

  subparsers$add_parser(
    "rate-limit",
    help = "Show the current OpenAlex API budget for OPENALEX_API_KEY"
  )

  prepare_oc <- subparsers$add_parser(
    "prepare-opencitations",
    help = paste("Load Retraction Watch seeds as DOI nodes for an",
                "OpenCitations-only run")
  )
  prepare_oc$add_argument("--csv", type = "character", default = NULL,
                          help = "Retraction Watch CSV path")
  prepare_oc_alias <- subparsers$add_parser(
    "prepare-oc",
    help = "Alias for prepare-opencitations"
  )
  prepare_oc_alias$add_argument("--csv", type = "character", default = NULL,
                                help = "Retraction Watch CSV path")

  crawl_oc <- subparsers$add_parser(
    "crawl-opencitations",
    help = "Run or resume the OpenCitations-only DOI citation crawl"
  )
  crawl_oc$add_argument("--max-depth", type = "integer", default = 3L)
  crawl_oc$add_argument("--complete-depth", type = "integer", default = 2L)
  crawl_oc$add_argument("--depth3-node-cap", type = "integer", default = 250000L)
  crawl_oc$add_argument("--parent-limit", type = "integer", default = NULL)
  crawl_oc_alias <- subparsers$add_parser(
    "crawl-oc",
    help = "Alias for crawl-opencitations"
  )
  crawl_oc_alias$add_argument("--max-depth", type = "integer", default = 3L)
  crawl_oc_alias$add_argument("--complete-depth", type = "integer", default = 2L)
  crawl_oc_alias$add_argument("--depth3-node-cap", type = "integer", default = 250000L)
  crawl_oc_alias$add_argument("--parent-limit", type = "integer", default = NULL)

  crawl <- subparsers$add_parser("crawl", help = "Run or resume citation crawl")
  crawl$add_argument("--max-depth", type = "integer", default = 3L)
  crawl$add_argument("--complete-depth", type = "integer", default = 2L)
  crawl$add_argument("--batch-size", type = "integer", default = 100L)
  crawl$add_argument("--per-page", type = "integer", default = 100L)
  crawl$add_argument("--depth3-node-cap", type = "integer", default = 250000L)
  crawl$add_argument("--depth3-page-cap", type = "integer", default = 2500L)
  crawl$add_argument("--no-opencitations", action = "store_true",
                     help = "Disable the default OpenCitations-first citation supplement")

  analyze <- subparsers$add_parser("analyze",
                                  help = "Generate tables, figures, and graph exports")
  analyze$add_argument("--max-analysis-depth", type = "integer", default = 2L)

  subparsers$add_parser("report", help = "Write outputs/report.md")

  manuscript_p <- subparsers$add_parser(
    "manuscript",
    help = "Build the reproducible manuscript (stats, figures, rendered docs)"
  )
  manuscript_p$add_argument("--no-render", action = "store_true",
                            help = "Compute stats and figures without rendering")
  manuscript_p$add_argument("--no-unique-d2", action = "store_true",
                            help = "Skip the unique post-notice depth-2 scan")

  run_all <- subparsers$add_parser("run-all",
                                   help = "Fetch, prepare, crawl, analyze, and report")
  run_all$add_argument("--max-depth", type = "integer", default = 3L)
  run_all$add_argument("--complete-depth", type = "integer", default = 2L)
  run_all$add_argument("--batch-size", type = "integer", default = 100L)
  run_all$add_argument("--per-page", type = "integer", default = 100L)
  run_all$add_argument("--depth3-node-cap", type = "integer", default = 250000L)
  run_all$add_argument("--depth3-page-cap", type = "integer", default = 2500L)
  run_all$add_argument("--no-opencitations", action = "store_true")
  run_all$add_argument("--title-fallback", action = "store_true",
                        help = paste("Use expensive title/author OpenAlex search",
                                     "after DOI and PMID resolution fail"))

  run_oc <- subparsers$add_parser(
    "run-opencitations",
    help = paste("Fetch, prepare, crawl, analyze, and report using",
                 "OpenCitations only")
  )
  run_oc$add_argument("--max-depth", type = "integer", default = 3L)
  run_oc$add_argument("--complete-depth", type = "integer", default = 2L)
  run_oc$add_argument("--depth3-node-cap", type = "integer", default = 250000L)
  run_oc$add_argument("--parent-limit", type = "integer", default = NULL)
  run_oc_alias <- subparsers$add_parser(
    "run-oc",
    help = "Alias for run-opencitations"
  )
  run_oc_alias$add_argument("--max-depth", type = "integer", default = 3L)
  run_oc_alias$add_argument("--complete-depth", type = "integer", default = 2L)
  run_oc_alias$add_argument("--depth3-node-cap", type = "integer", default = 250000L)
  run_oc_alias$add_argument("--parent-limit", type = "integer", default = NULL)

  parser
}

#' CLI entry point.
#'
#' Parses `argv`, constructs a `Settings` instance from the global flags,
#' dispatches to the selected subcommand's `cmd_*` helper (resolved via
#' `SUBCOMMAND_DISPATCH` because the R `argparse` package cannot store
#' closures in the parsed namespace), and returns an integer exit code.
#'
#' @param argv character vector of command-line tokens, defaulting to
#'   `commandArgs(trailingOnly = TRUE)`.
#' @return integer exit code (0 on success, 2 on missing subcommand,
#'   75 on OpenAlex rate-limit error, 130 on Ctrl-C).
#' @export
main <- function(argv = commandArgs(trailingOnly = TRUE)) {
  parser <- build_parser()
  args <- parser$parse_args(argv)
  command <- args$command
  if (is.null(command) || is.na(command) || command == "") {
    parser$print_help()
    return(2L)
  }
  func <- SUBCOMMAND_DISPATCH[[command]]
  if (is.null(func)) {
    parser$print_help()
    return(2L)
  }
  settings <- Settings_from_env(args$data_dir, args$output_dir, args$db)
  tryCatch(
    func(args, settings),
    OpenAlexRateLimitError = function(e) {
      message(conditionMessage(e))
      75L
    },
    interrupt = function(e) {
      message("Interrupted.")
      130L
    }
  )
}

if (sys.nframe() == 0L) {
  quit(status = main())
}
