#' @title OpenCitations API client
#' @description Port of the original Python `retraction_pollution.opencitations`
#'   module. Provides an HTTP client for the OpenCitations index and metadata
#'   endpoints, plus helpers for building citation-graph node IDs and parsing
#'   raw OpenCitation records into normalized R objects.
#'
#'   The pure helpers (`doi_node_id`, `pmid_node_id`, `doi_from_node_id`,
#'   `extract_pid`, `parse_open_citation`) depend only on the `util.R`
#'   cleaners (`clean_doi`, `clean_pmid`, `parse_date`), which live in the
#'   same package namespace.

OPENCITATIONS_INDEX_API <- "https://api.opencitations.net/index/v2"
OPENCITATIONS_META_API <- "https://api.opencitations.net/meta/v1"

#' Construct an `OpenCitationsError` condition object.
#' @param msg character message.
#' @return An object of class `c("OpenCitationsError", "error", "condition")`.
open_citations_error <- function(msg) {
  structure(
    list(message = msg),
    class = c("OpenCitationsError", "error", "condition")
  )
}

#' Raise an `OpenCitationsError`.
#' @param msg character message.
#' @return Never; signals an error.
stop_open_citations <- function(msg) {
  stop(open_citations_error(msg))
}

#' Construct an `OpenCitation` data-carrier object.
#'
#' A simple list-based S3 object holding the normalized fields of a single
#' OpenCitations record plus the raw source list.
#'
#' @param citing_doi character (possibly `NA_character_`)
#' @param citing_pmid character (possibly `NA_character_`)
#' @param cited_doi character (possibly `NA_character_`)
#' @param creation_date character ISO date (possibly `NA_character_`)
#' @param raw list, the original record.
#' @return An `OpenCitation` object.
OpenCitation <- function(citing_doi = NA_character_,
                         citing_pmid = NA_character_,
                         cited_doi = NA_character_,
                         creation_date = NA_character_,
                         raw = list()) {
  structure(
    list(
      citing_doi = citing_doi,
      citing_pmid = citing_pmid,
      cited_doi = cited_doi,
      creation_date = creation_date,
      raw = raw
    ),
    class = "OpenCitation"
  )
}

#' OpenCitations HTTP client.
#'
#' List-based class wrapping `httr2` requests against the OpenCitations
#' index (`/citations/doi:<doi>`) and metadata (`/metadata/doi:<doi>`)
#' endpoints. Retries on 429 and 5xx, treats 404 as an empty result, and
#' sleeps `request_delay` seconds between requests.
#'
#' @param token character OpenCitations access token, or `NULL`.
#' @param index_base_url character base URL for the index API.
#' @param meta_base_url character base URL for the metadata API.
#' @param retries integer max retry attempts.
#' @param request_delay numeric seconds to sleep before each request.
#' @param request_timeout numeric seconds before a request is aborted.
#' @return An `OpenCitationsClient` object.
OpenCitationsClient <- function(token = NULL,
                                index_base_url = OPENCITATIONS_INDEX_API,
                                meta_base_url = OPENCITATIONS_META_API,
                                retries = 5L,
                                request_delay = 0.2,
                                request_timeout = 60) {
  index_base_url <- sub("/+$", "", index_base_url)
  meta_base_url <- sub("/+$", "", meta_base_url)

  client <- structure(
    list(
      token = token,
      index_base_url = index_base_url,
      meta_base_url = meta_base_url,
      retries = as.integer(retries),
      request_delay = as.numeric(request_delay),
      request_timeout = as.numeric(request_timeout)
    ),
    class = "OpenCitationsClient"
  )

  # Perform an HTTP GET, returning parsed JSON.
  # 404 -> empty list; 429/5xx retried up to `retries` times.
  request_json <- function(url) {
    if (client$request_delay > 0) {
      Sys.sleep(client$request_delay)
    }
    req <- httr2::request(url)
    req <- httr2::req_headers(req, Accept = "application/json")
    if (!is.na(client$request_timeout) && client$request_timeout > 0) {
      req <- httr2::req_timeout(req, client$request_timeout)
    }
    if (!is.null(client$token) && !is.na(client$token) &&
        nchar(client$token) > 0) {
      req <- httr2::req_headers(req, Authorization = client$token)
    }
    req <- httr2::req_retry(req,
                            max_tries = client$retries,
                            max_seconds = 60,
                            is_transient = function(resp) {
                              httr2::resp_status(resp) == 429 ||
                                httr2::resp_status(resp) >= 500
                            })
    req <- httr2::req_error(req, is_error = function(resp) FALSE)

    resp <- tryCatch(
      httr2::req_perform(req),
      httr2_error = function(e) {
        stop_open_citations(paste("OpenCitations request failed:", conditionMessage(e)))
      }
    )

    status <- httr2::resp_status(resp)
    if (status == 404) {
      return(list())
    }
    if (status >= 400) {
      stop_open_citations(paste0("OpenCitations HTTP ", status))
    }
    httr2::resp_body_json(resp)
  }

  client$citations_by_doi <- function(doi) {
    cleaned <- clean_doi(doi)
    if (is.na(cleaned)) return(list())
    url <- paste0(index_base_url, "/citations/doi:",
                  utils::URLencode(cleaned, reserved = TRUE))
    data <- request_json(url)
    if (!is.list(data)) return(list())
    lapply(data, function(item) {
      if (is.list(item)) parse_open_citation(item) else NULL
    })
  }

  client$metadata_by_doi <- function(doi) {
    cleaned <- clean_doi(doi)
    if (is.na(cleaned)) return(NULL)
    url <- paste0(meta_base_url, "/metadata/doi:",
                  utils::URLencode(cleaned, reserved = TRUE))
    data <- request_json(url)
    if (is.list(data) && length(data) > 0) data[[1]] else NULL
  }

  client
}

#' Build a `doi:<doi>` node ID from a DOI value.
#' @param doi any R object.
#' @return `paste0("doi:", clean_doi(doi))` or `NA_character_`.
doi_node_id <- function(doi) {
  cleaned <- clean_doi(doi)
  if (is.na(cleaned)) return(NA_character_)
  paste0("doi:", cleaned)
}

#' Build a `pmid:<pmid>` node ID from a PMID value.
#' @param pmid any R object.
#' @return `paste0("pmid:", clean_pmid(pmid))` or `NA_character_`.
pmid_node_id <- function(pmid) {
  cleaned <- clean_pmid(pmid)
  if (is.na(cleaned)) return(NA_character_)
  paste0("pmid:", cleaned)
}

#' Extract the bare DOI from a `doi:<doi>` node ID.
#' @param node_id character (possibly `NA`).
#' @return bare lowercase DOI or `NA_character_`.
doi_from_node_id <- function(node_id) {
  if (is_missing(node_id)) return(NA_character_)
  text <- as.character(node_id)
  if (is.na(text) || !startsWith(text, "doi:")) return(NA_character_)
  clean_doi(substr(text, 5, nchar(text)))
}

#' Extract a PID of the given prefix from a space-separated PID string.
#'
#' Matches `(?:^|\\s)<prefix>:(\\S+)` case-insensitively, returning the
#' captured value or `NA_character_`.
#'
#'
#' The `prefix` is regex-escaped so that unusual prefixes are matched
#' literally (no accidental metacharacter interpretation).
#'
#' @param pid_string character (possibly `NA`).
#' @param prefix character, e.g. `"doi"`, `"pmid"`, `"pmcid"`.
#' @return captured PID string or `NA_character_`.
#' @noRd
regex_escape <- function(x) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}

extract_pid <- function(pid_string, prefix) {
  if (is_missing(pid_string)) return(NA_character_)
  text <- as.character(pid_string)
  if (is.na(text)) return(NA_character_)
  pattern <- paste0("(?:^|\\s)", regex_escape(prefix), ":([^\\s]+)")
  m <- stringr::str_match(text, pattern)
  if (is.na(m[1, 1])) return(NA_character_)
  m[1, 2]
}

#' Parse a raw OpenCitation record list into an `OpenCitation` object.
#' @param item list with fields `citing`, `cited`, `creation`.
#' @return an `OpenCitation` object.
parse_open_citation <- function(item) {
  OpenCitation(
    citing_doi = clean_doi(extract_pid(item$citing, "doi")),
    citing_pmid = clean_pmid(extract_pid(item$citing, "pmid")),
    cited_doi = clean_doi(extract_pid(item$cited, "doi")),
    creation_date = parse_date(item$creation),
    raw = item
  )
}
