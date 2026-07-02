#' @title Run metadata helpers
#' @description Helpers for recording source snapshot and access metadata in
#'   the DuckDB `run_metadata` table.

#' @noRd
rw_snapshot_label <- function(path) {
  if (is_missing(path) || is.na(path) || !file.exists(path)) return("unknown")
  base <- basename(path)
  matched <- regmatches(base, regexpr("\\d{8}T\\d{6}Z", base))
  if (length(matched) == 1L && nzchar(matched)) return(matched)
  mtime <- file.info(path)$mtime
  if (is.na(mtime)) return("unknown")
  format(mtime, "%Y-%m-%d %H:%M:%S %Z")
}

#' Record data-source metadata for a run.
#'
#' @param store StudyStore.
#' @param rw_path Retraction Watch CSV path, or `NA`.
#' @param pipeline_mode character.
#' @param uses_opencitations logical.
#' @param uses_openalex logical.
#' @return invisible NULL.
record_source_metadata <- function(store, rw_path = NA,
                                   pipeline_mode = "unknown",
                                   uses_opencitations = FALSE,
                                   uses_openalex = FALSE) {
  store$set_metadata("pipeline_mode", pipeline_mode)
  if (!is_missing(rw_path) && !is.na(rw_path)) {
    store$set_metadata("rw_snapshot_date", rw_snapshot_label(rw_path))
  }
  if (isTRUE(uses_opencitations)) {
    store$set_metadata("oc_access_date", as.character(Sys.Date()))
  }
  if (isTRUE(uses_openalex)) {
    store$set_metadata("openalex_access_date", as.character(Sys.Date()))
  } else {
    store$set_metadata("openalex_access_date", "not used")
  }
  invisible(NULL)
}

