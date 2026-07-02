#' @title Retraction Watch data acquisition
#' @description Download, parse, and seed-row building for the Retraction Watch
#'   CSV export, ported from the original Python pipeline
#'   (`retraction_pollution.rw`).

RETRACTION_WATCH_URL <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv?ref_type=heads&inline=false"

NOTICE_TYPES <- c("retraction", "expression of concern")

#' Download the Retraction Watch CSV.
#'
#' @param raw_dir character path to the raw data directory (created if needed).
#' @param url source URL, defaulting to `RETRACTION_WATCH_URL`.
#' @param filename optional filename; if `NULL`, a UTC timestamped name of the
#'   form `retraction_watch_YYYYmmddTHHMMSSZ.csv` is generated.
#' @return character path to the downloaded file.
download_retraction_watch <- function(raw_dir,
                                      url = RETRACTION_WATCH_URL,
                                      filename = NULL) {
  ensure_dir(raw_dir)
  if (is.null(filename) || is.na(filename) || filename == "") {
    stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    filename <- paste0("retraction_watch_", stamp, ".csv")
  }
  output_path <- file.path(raw_dir, filename)
  request <- httr2::request(url)
  request <- httr2::req_headers(request,
                                `User-Agent` = "RetractionPollution/0.1 (research pipeline)")
  resp <- httr2::req_perform(request)
  writeBin(httr2::resp_body_raw(resp), output_path)
  latest_path <- file.path(raw_dir, "retraction_watch_latest.csv")
  file.copy(output_path, latest_path, overwrite = TRUE)
  output_path
}

#' Read the Retraction Watch CSV as a tibble of character columns.
#'
#' Column names and cell values are trimmed. UTF-8 decoding errors are
#' replaced (readr default behavior) so a single malformed row never aborts
#' the whole pipeline run.
#'
#' @param path character path to the CSV.
#' @return a tibble with all character columns.
iter_retraction_watch_rows <- function(path) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    show_col_types = FALSE
  )
  names(df) <- stringr::str_trim(names(df))
  for (col in names(df)) {
    df[[col]] <- stringr::str_trim(as.character(df[[col]]))
  }
  df
}

#' Is the RetractionNature value a seed notice (retraction or EOC)?
#'
#' @param nature character (possibly `NA`).
#' @return logical.
is_seed_notice <- function(nature) {
  if (is_missing(nature)) return(FALSE)
  text <- tolower(as.character(nature))
  if (is.na(text) || text == "") return(FALSE)
  stringr::str_detect(text, paste(NOTICE_TYPES, collapse = "|"))
}

#' Convert a row (named list) into a seed record.
#'
#' @param row named list of character fields (one CSV row).
#' @return named list seed.
row_to_seed <- function(row) {
  record_id <- row[["Record ID"]]
  if (is_missing(record_id) || record_id == "") {
    record_id <- row[["RecordID"]]
  }
  if (is_missing(record_id)) record_id <- ""
  list(
    record_id = record_id,
    title = text_or_none(row[["Title"]]),
    notice_type = text_or_none(row[["RetractionNature"]]),
    notice_date = parse_date(row[["RetractionDate"]]),
    original_paper_date = parse_date(row[["OriginalPaperDate"]]),
    original_doi = clean_doi(row[["OriginalPaperDOI"]]),
    original_pmid = clean_pmid(row[["OriginalPaperPubMedID"]]),
    author = text_or_none(row[["Author"]]),
    journal = text_or_none(row[["Journal"]]),
    publisher = text_or_none(row[["Publisher"]]),
    subject = text_or_none(row[["Subject"]]),
    reason = text_or_none(row[["Reason"]]),
    article_type = text_or_none(row[["ArticleType"]]),
    country = text_or_none(row[["Country"]]),
    openalex_id = NA_character_,
    resolved_by = NA_character_,
    resolved_status = "pending",
    source_row_json = json_dumps(row)
  )
}

#' Load seed rows from the Retraction Watch CSV.
#'
#' Reads the CSV, keeps only rows whose `RetractionNature` is a seed notice
#' and whose `record_id` is non-empty, and returns a list of seed lists.
#'
#' @param path character path to the CSV.
#' @return list of named lists (one per seed).
load_seed_rows <- function(path) {
  df <- iter_retraction_watch_rows(path)
  if (!"RetractionNature" %in% names(df)) {
    return(list())
  }
  keep <- vapply(df[["RetractionNature"]], is_seed_notice, logical(1))
  df <- df[keep, , drop = FALSE]
  rows <- purrr::transpose(df)
  seeds <- list()
  for (row in rows) {
    row <- lapply(row, function(x) if (is.null(x)) NA_character_ else x)
    seed <- row_to_seed(row)
    if (!is_missing(seed$record_id) && seed$record_id != "") {
      seeds <- c(seeds, list(seed))
    }
  }
  seeds
}

#' Build fallback search terms from a seed.
#'
#' @param seed named list (output of `row_to_seed`).
#' @return named list with `title` and `author_last_name`.
search_fallback_terms <- function(seed) {
  list(
    title = text_or_none(seed$title),
    author_last_name = first_author_last_name(seed$author)
  )
}