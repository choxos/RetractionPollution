#' @title Configuration for retractionpollution
#' @description Port of the original Python `retraction_pollution.config`
#'   module. Provides a `Settings` class (R6) with environment-driven
#'   OpenAlex/OpenCitations credentials and derived directory paths.

library(R6)

#' Retraction Watch CSV source URL.
#'
#' Same constant as in `rw.R`; re-exported here so callers that source only
#' `config.R` (e.g. the CLI help text) see a single source of truth.
RETRACTION_WATCH_URL <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv?ref_type=heads&inline=false"

#' @title Settings
#' @description R6 class holding pipeline configuration. Fields are populated
#'   from constructor arguments or environment variables; the `raw_dir`,
#'   `processed_dir`, `figure_dir`, `table_dir`, and `graph_dir` active
#'   bindings derive paths from `data_dir` / `output_dir`.
#' @noRd
Settings <- R6::R6Class(
  "Settings",
  cloneable = FALSE,
  public = list(
    data_dir = NULL,
    output_dir = NULL,
    db_path = NULL,
    openalex_api_key = NULL,
    openalex_email = NULL,
    openalex_request_delay = NULL,
    openalex_rate_limit_sleep = NULL,
    opencitations_token = NULL,

    initialize = function(data_dir = "data", output_dir = "outputs",
                          db_path = NA,
                          openalex_api_key = Sys.getenv("OPENALEX_API_KEY", ""),
                          openalex_email = Sys.getenv("OPENALEX_EMAIL", ""),
                          openalex_request_delay = as.numeric(
                            Sys.getenv("OPENALEX_REQUEST_DELAY", "0.35")),
                          openalex_rate_limit_sleep = as.numeric(
                            Sys.getenv("OPENALEX_RATE_LIMIT_SLEEP", "60")),
                          opencitations_token = Sys.getenv(
                            "OPENCITATIONS_TOKEN", "")) {
      self$data_dir <- data_dir
      self$output_dir <- output_dir
      self$db_path <- if (is_missing(db_path) || is.null(db_path) ||
                          (is.character(db_path) && length(db_path) == 1L &&
                           is.na(db_path))) {
        file.path(data_dir, "processed", "study.duckdb")
      } else {
        db_path
      }
      self$openalex_api_key <- openalex_api_key
      self$openalex_email <- openalex_email
      self$openalex_request_delay <- openalex_request_delay
      self$openalex_rate_limit_sleep <- openalex_rate_limit_sleep
      self$opencitations_token <- opencitations_token
      invisible(self)
    }
  ),

  active = list(
    raw_dir = function() file.path(self$data_dir, "raw"),
    processed_dir = function() file.path(self$data_dir, "processed"),
    figure_dir = function() file.path(self$output_dir, "figures"),
    table_dir = function() file.path(self$output_dir, "tables"),
    graph_dir = function() file.path(self$output_dir, "graphs")
  )
)

#' Build a `Settings` instance from environment variables.
#'
#' Class-style convenience constructor mirroring Python's
#' `Settings.from_env(...)`.
#'
#' @param data_dir character data directory, default `"data"`.
#' @param output_dir character output directory, default `"outputs"`.
#' @param db_path character DuckDB path, or `NA` to use
#'   `file.path(data_dir, "processed", "study.duckdb")`.
#' @return A `Settings` instance.
#' @export
Settings_from_env <- function(data_dir = "data", output_dir = "outputs",
                              db_path = NA) {
  Settings$new(
    data_dir = data_dir,
    output_dir = output_dir,
    db_path = db_path,
    openalex_api_key = Sys.getenv("OPENALEX_API_KEY", ""),
    openalex_email = Sys.getenv("OPENALEX_EMAIL", ""),
    openalex_request_delay = as.numeric(
      Sys.getenv("OPENALEX_REQUEST_DELAY", "0.35")),
    openalex_rate_limit_sleep = as.numeric(
      Sys.getenv("OPENALEX_RATE_LIMIT_SLEEP", "60")),
    opencitations_token = Sys.getenv("OPENCITATIONS_TOKEN", "")
  )
}

#' Return settings overridden for an OpenCitations-only run.
#'
#' If `args$db` is `NA`/`NULL`, the DuckDB path is set to
#' `file.path(settings$processed_dir(), "opencitations.duckdb")`. If
#' `args$output_dir == "outputs"`, the output directory is redirected to
#' `file.path("outputs", "opencitations")`.
#'
#' @param args list/namespace with `db` and `output_dir` elements.
#' @param settings `Settings` instance.
#' @return A new `Settings` instance with overrides applied.
opencitations_only_settings <- function(args, settings) {
  db_path <- settings$db_path
  if (is_missing(args$db) || is.null(args$db) || (is.character(args$db) &&
      length(args$db) == 1L && is.na(args$db))) {
    db_path <- file.path(settings$processed_dir, "opencitations.duckdb")
  }
  output_dir <- settings$output_dir
  if (identical(as.character(args$output_dir), "outputs")) {
    output_dir <- file.path("outputs", "opencitations")
  }
  Settings$new(
    data_dir = settings$data_dir,
    output_dir = output_dir,
    db_path = db_path,
    openalex_api_key = settings$openalex_api_key,
    openalex_email = settings$openalex_email,
    openalex_request_delay = settings$openalex_request_delay,
    openalex_rate_limit_sleep = settings$openalex_rate_limit_sleep,
    opencitations_token = settings$opencitations_token
  )
}