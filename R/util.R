#' @title Utility functions for retractionpollution
#' @description Helpers for DOI, PMID, date, and OpenAlex ID normalization,
#'   ported from the original Python pipeline (`retraction_pollution.util`).
#'   The critical fix here is `parse_date`, which now correctly handles the
#'   `"M/D/YYYY H:M"` format emitted by the Retraction Watch CSV export.

DOI_UNAVAILABLE <- c("", "unavailable", "n/a", "na", "none", "null", "0")

#' Missingness check
#' @param value any R object
#' @return `TRUE` if value is `NA`, `NaN`, or length 0, else `FALSE`.
is_missing <- function(value) {
  if (is.null(value)) return(TRUE)
  if (length(value) == 0) return(TRUE)
  if (is.nan(value)) return(TRUE)
  if (anyNA(value)) return(TRUE)
  FALSE
}

#' Normalize text, returning `NA_character_` for missing/empty input.
#' @param value any R object
#' @return trimmed character string or `NA_character_`.
text_or_none <- function(value) {
  if (is_missing(value)) return(NA_character_)
  text <- as.character(value)
  text <- stringr::str_trim(text)
  if (is.na(text) || text == "") return(NA_character_)
  text
}

#' Normalize DOI to bare lowercase.
#' Strips `https://doi.org/`, `http://dx.doi.org/`, and `doi:` prefixes,
#' extracts via regex, and rejects unavailable sentinels.
#' @param value any R object
#' @return normalized lowercase DOI or `NA_character_`.
clean_doi <- function(value) {
  text <- text_or_none(value)
  if (is.na(text)) return(NA_character_)
  doi <- stringr::str_trim(text)
  doi <- gsub("^[.\\s]+|[.\\s]+$", "", doi)
  doi <- gsub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi <- gsub("^doi:\\s*", "", doi, ignore.case = TRUE)
  m <- stringr::str_match(doi, "(10\\.\\d{4,9}/\\S+)")
  if (!is.na(m[1, 2])) doi <- m[1, 2]
  doi <- gsub("^[.\\s;,:]+|[.\\s;,:]+$", "", doi)
  if (is.na(doi) || doi == "" || tolower(doi) %in% DOI_UNAVAILABLE) {
    return(NA_character_)
  }
  tolower(doi)
}

#' Build a DOI URL from a value.
#' @param value any R object
#' @return `https://doi.org/<doi>` or `NA_character_`.
doi_url <- function(value) {
  doi <- clean_doi(value)
  if (is.na(doi)) return(NA_character_)
  paste0("https://doi.org/", doi)
}

#' Extract numeric PMID.
#' Rejects `"0"`, `"0.0"`, `"unavailable"`, `"n/a"`, `"na"`.
#' @param value any R object
#' @return PMID string or `NA_character_`.
clean_pmid <- function(value) {
  pmid <- text_or_none(value)
  if (is.na(pmid)) return(NA_character_)
  if (pmid == "" || pmid %in% c("0", "0.0") ||
      tolower(pmid) %in% c("unavailable", "n/a", "na")) {
    return(NA_character_)
  }
  m <- stringr::str_extract(pmid, "\\d+")
  if (is.na(m)) return(NA_character_)
  m
}

#' Parse a date string into ISO `YYYY-MM-DD` form.
#'
#' THE CRITICAL FIX: handles the `"M/D/YYYY 0:00"` format emitted by
#' the Retraction Watch CSV, which the original Python implementation
#' failed on (breaking all post-notice analysis).
#'
#' Supported formats:
#' - `"1/21/2026 0:00"` (M/D/YYYY H:M) — Retraction Watch format
#' - `"10/15/2022 0:00"` (MM/DD/YYYY H:M)
#' - `"2026-01-21"` (ISO)
#' - `"2026-01"` (year-month → pad to `-01`)
#' - `"2026"` (year → pad to `-01-01`)
#' - `"1/21/2026"` (M/D/YYYY no time)
#' - `"2026/01/21"` (Y/M/D)
#'
#' Returns `NA_character_` for `"0000-00-00"`, `"0"`, `""`, `NA`.
#'
#' @param value any R object
#' @return ISO date string or `NA_character_`.
parse_date <- function(value) {
  text <- text_or_none(value)
  if (is.na(text)) return(NA_character_)
  if (text == "" || text %in% c("0000-00-00", "0")) return(NA_character_)

  # Strip any trailing time component; take the date part before the space.
  date_part <- strsplit(text, " ", fixed = TRUE)[[1]][1]
  if (is.na(date_part) || date_part == "") return(NA_character_)

  # Year-month: pad to -01.
  if (grepl("^\\d{4}-\\d{2}$", date_part)) {
    return(paste0(date_part, "-01"))
  }
  # Year only: pad to -01-01.
  if (grepl("^\\d{4}$", date_part)) {
    return(paste0(date_part, "-01-01"))
  }

  # Try lubridate with the relevant orders.
  parsed <- suppressWarnings(
    lubridate::parse_date_time(date_part,
                               orders = c("Ymd", "mdY", "dmY"),
                               quiet = TRUE)
  )
  if (!is.na(parsed)) {
    return(format(as.Date(parsed), "%Y-%m-%d"))
  }

  # Fall back to other orders: Ym and Y are handled above via regex,
  # but lubridate can also produce POSIXct for them; covered already.
  NA_character_
}

#' Compact an OpenAlex ID by stripping the `https://openalex.org/` prefix.
#' @param value any R object
#' @return short ID (e.g. `"W123456"`) or `NA_character_`.
compact_openalex_id <- function(value) {
  text <- text_or_none(value)
  if (is.na(text)) return(NA_character_)
  if (startsWith(text, "https://openalex.org/")) {
    return(sub("^https://openalex.org/", "", text))
  }
  text
}

#' Expand a compact OpenAlex ID to the full `https://openalex.org/<id>` form.
#' @param value any R object
#' @return full URL or `NA_character_`.
full_openalex_id <- function(value) {
  compact <- compact_openalex_id(value)
  if (is.na(compact)) return(NA_character_)
  paste0("https://openalex.org/", compact)
}

#' Serialize an R object to a JSON string.
#' @param value any R object
#' @return JSON character string.
json_dumps <- function(value) {
  s <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null",
                        na = "null", pretty = FALSE)
  as.character(s)
}

#' Deterministic, order-independent hash of a character vector.
#'
#' Sorts the input before hashing so `stable_hash(c("a","b"))` equals
#' `stable_hash(c("b","a"))`. Uses base R bitwise ops on UTF-8 bytes
#' (no external digest dependency required). Sufficient for deduplication
#' keys such as crawl job IDs.
#'
#' @param values character vector
#' @return 8-hex-character string.
stable_hash <- function(values) {
  payload <- paste(sort(values), collapse = "\n")
  bytes <- charToRaw(enc2utf8(payload))
  h <- 0L
  for (b in bytes) {
    h <- bitwXor(bitwShiftL(h, 5), bitwShiftL(h, 13))
    h <- bitwXor(h, as.integer(b))
    h <- bitwAnd(h, 0x7FFFFFFFL)
  }
  sprintf("%08x", h)
}

#' Split a vector into chunks of at most `size` elements.
#' @param items vector
#' @param size integer > 0
#' @return list of vectors, each of length `size` (last may be shorter).
chunked <- function(items, size) {
  if (length(items) == 0) return(list())
  n <- ceiling(length(items) / size)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    start <- (i - 1) * size + 1
    end <- min(i * size, length(items))
    out[[i]] <- items[start:end]
  }
  out
}

#' Extract the last name of the first author from a semicolon-separated field.
#' @param author_field character
#' @return last name string or `NA_character_`.
first_author_last_name <- function(author_field) {
  text <- text_or_none(author_field)
  if (is.na(text)) return(NA_character_)
  first_author <- stringr::str_trim(strsplit(text, ";", fixed = TRUE)[[1]][1])
  if (is.na(first_author) || first_author == "") return(NA_character_)
  pieces <- stringr::str_split(stringr::str_trim(first_author), "\\s+")[[1]]
  pieces <- pieces[pieces != ""]
  if (length(pieces) == 0) return(NA_character_)
  pieces[length(pieces)]
}

#' Create a directory (recursively) if needed.
#' @param path character
#' @return the path, invisibly.
ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}