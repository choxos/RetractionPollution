#' @title OpenAlex API client
#' @description Port of the original Python `retraction_pollution.openalex`
#'   module. Provides an HTTP client for the OpenAlex REST API (`/works`,
#'   `/works/pmid:<pmid>`) plus helpers for normalizing raw work records into
#'   the storage schema and extracting citation edges from `referenced_works`.
#'
#'   The pure helpers (`normalize_work`, `edges_from_work`, `openalex_url`)
#'   depend only on the `util.R` cleaners (`clean_doi`, `compact_openalex_id`,
#'   `doi_url`, `full_openalex_id`, `json_dumps`, `text_or_none`).

library(R6)

OPENALEX_API <- "https://api.openalex.org"

WORK_SELECT <- paste(
  c("id", "doi", "display_name", "title", "publication_date",
    "publication_year", "type", "type_crossref", "is_retracted",
    "cited_by_count", "referenced_works", "primary_location",
    "primary_topic", "topics"),
  collapse = ","
)

RESOLUTION_SELECT <- paste(
  c("id", "doi", "display_name", "title", "publication_date",
    "publication_year", "type", "is_retracted", "cited_by_count",
    "referenced_works", "primary_location", "primary_topic", "topics",
    "authorships"),
  collapse = ","
)

# Null-coalescing helper (matches Python's `x or default`).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' Construct an `OpenAlexError` condition object (without raising).
#' @param msg character message.
#' @param status_code integerish HTTP status, or `NA`.
#' @param response_text character response body, or `NA`.
#' @return An object of class `c("OpenAlexError", "error", "condition")`.
new_openalex_error <- function(msg, status_code = NA_integer_,
                              response_text = NA_character_) {
  structure(
    list(message = msg, status_code = status_code,
         response_text = response_text),
    class = c("OpenAlexError", "error", "condition")
  )
}

#' Raise an `OpenAlexError`.
#' @inheritParams new_openalex_error
#' @return Never; signals an error of class `OpenAlexError`.
openalex_error <- function(msg, status_code = NA_integer_,
                           response_text = NA_character_) {
  stop(new_openalex_error(msg, status_code = status_code,
                         response_text = response_text))
}

#' Raise an `OpenAlexError`.
#' @noRd
stop_openalex <- function(msg, status_code = NA_integer_,
                          response_text = NA_character_) {
  openalex_error(msg, status_code = status_code,
                 response_text = response_text)
}

#' Construct an `OpenAlexRateLimitError` condition object (without raising).
#' @inheritParams new_openalex_error
#' @return An object of class
#'   `c("OpenAlexRateLimitError", "OpenAlexError", "error", "condition")`.
new_openalex_rate_limit_error <- function(msg, status_code = 429L,
                                          response_text = NA_character_) {
  structure(
    list(message = msg, status_code = status_code,
         response_text = response_text),
    class = c("OpenAlexRateLimitError", "OpenAlexError", "error", "condition")
  )
}

#' Raise an `OpenAlexRateLimitError`.
#' @inheritParams new_openalex_error
#' @return Never; signals an error of class `OpenAlexRateLimitError`.
openalex_rate_limit_error <- function(msg, status_code = 429L,
                                       response_text = NA_character_) {
  stop(new_openalex_rate_limit_error(msg, status_code = status_code,
                                     response_text = response_text))
}

#' Raise an `OpenAlexRateLimitError`.
#' @noRd
stop_openalex_rate_limit <- function(msg, status_code = 429L,
                                     response_text = NA_character_) {
  openalex_rate_limit_error(msg, status_code = status_code,
                            response_text = response_text)
}

#' Construct an `OpenAlexPage` data-carrier object.
#'
#' @param results list of raw work records.
#' @param next_cursor character cursor for the next page, or `NULL`/`NA`.
#' @param count integerish total result count, or `NA`.
#' @return An `OpenAlexPage` object.
OpenAlexPage <- function(results = list(), next_cursor = NULL,
                         count = NA_integer_) {
  structure(
    list(
      results = results,
      next_cursor = if (is_missing(next_cursor)) NULL else next_cursor,
      count = count
    ),
    class = "OpenAlexPage"
  )
}

#' @title OpenAlex HTTP client (R6)
#' @description Wraps `httr2` requests against the OpenAlex `/works` endpoint.
#'   Retries on 429 and 5xx, treats 404 as an empty result, and sleeps
#'   `request_delay` seconds before each request. The private `request_json`
#'   method is overridable by subclasses for testing.
#' @noRd
OpenAlexClient <- R6::R6Class(
  "OpenAlexClient",
  cloneable = FALSE,
  public = list(
    api_key = NULL,
    email = NULL,
    base_url = NULL,
    retries = NULL,
    request_delay = NULL,
    rate_limit_sleep = NULL,

    initialize = function(api_key = NA_character_, email = NA_character_,
                          base_url = OPENALEX_API, retries = 6L,
                          request_delay = 0.35, rate_limit_sleep = 60.0) {
      self$api_key <- if (is_missing(api_key)) NA_character_ else api_key
      self$email <- if (is_missing(email)) NA_character_ else email
      self$base_url <- sub("/+$", "", base_url)
      self$retries <- as.integer(retries)
      self$request_delay <- as.numeric(request_delay)
      self$rate_limit_sleep <- as.numeric(rate_limit_sleep)
      invisible(self)
    },

    get_works_by_dois = function(dois) {
      values <- unique(vapply(dois, function(d) {
        d2 <- clean_doi(d)
        if (is.na(d2) || grepl(",", d2, fixed = TRUE) ||
            grepl("&", d2, fixed = TRUE)) return(NA_character_)
        doi_url(d2)
      }, character(1)))
      values <- values[!is.na(values)]
      if (length(values) == 0L) return(list())
      if (length(values) > 100L) {
        out <- list()
        for (start in seq(1L, length(values), by = 100L)) {
          end <- min(start + 99L, length(values))
          out <- c(out, private$get_works_by_doi_values(values[start:end]))
        }
        out
      } else {
        private$get_works_by_doi_values(values)
      }
    },

    get_work_by_pmid = function(pmid) {
      pmid_text <- clean_pmid(pmid)
      if (is.na(pmid_text)) return(NULL)
      data <- private$request_json(
        paste0("/works/pmid:", utils::URLencode(pmid_text, reserved = TRUE)),
        list(select = RESOLUTION_SELECT)
      )
      if (is.list(data) && !is.null(data$id) && !is.na(data$id)) data else NULL
    },

    search_work = function(title, author_last_name = NULL) {
      title_text <- text_or_none(title)
      if (is.na(title_text)) return(NULL)
      data <- private$request_json("/works", list(
        search = substr(title_text, 1, 200),
        `per-page` = 5L,
        select = RESOLUTION_SELECT
      ))
      candidates <- data$results %||% list()
      if (length(candidates) == 0L) return(NULL)
      author <- text_or_none(author_last_name)
      if (!is.na(author)) {
        needle <- tolower(author)
        for (work in candidates) {
          authorships <- work$authorships %||% list()
          names <- vapply(authorships, function(a) {
            au <- a$author %||% list()
            tolower(au$display_name %||% "")
          }, character(1))
          if (any(stringr::str_detect(names, fixed(needle)))) return(work)
        }
      }
      candidates[[1]]
    },

    list_citers = function(parent_ids, cursor = "*", per_page = 100L) {
      compact_ids <- vapply(parent_ids, compact_openalex_id, character(1))
      compact_ids <- compact_ids[!is.na(compact_ids) & compact_ids != ""]
      if (length(compact_ids) == 0L) return(OpenAlexPage(list(), NULL, 0L))
      if (length(compact_ids) > 100L) {
        stop("OpenAlex OR filters support at most 100 values per request.",
             call. = FALSE)
      }
      data <- private$request_json("/works", list(
        filter = paste0("cites:", paste(compact_ids, collapse = "|")),
        `per-page` = max(1L, min(100L, as.integer(per_page))),
        cursor = cursor,
        select = WORK_SELECT
      ))
      meta <- data$meta %||% list()
      OpenAlexPage(
        results = data$results %||% list(),
        next_cursor = meta$next_cursor,
        count = meta$count %||% NA_integer_
      )
    },

    rate_limit_status = function() {
      private$request_json("/rate-limit")
    }
  ),

  private = list(
    request_json = function(path, params = list()) {
      if (self$request_delay > 0) Sys.sleep(self$request_delay)
      url <- paste0(self$base_url, path)
      query <- as.list(params)
      if (!is.na(self$api_key) && nchar(self$api_key) > 0) {
        query$api_key <- self$api_key
      }
      if (!is.na(self$email) && nchar(self$email) > 0) {
        query$mailto <- self$email
      }
      req <- httr2::request(url)
      if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)
      req <- httr2::req_headers(
        req,
        Accept = "application/json",
        `User-Agent` = "RetractionPollution/0.1 (mailto optional; OpenAlex research)"
      )
      req <- httr2::req_retry(
        req,
        max_tries = self$retries,
        max_seconds = 60,
        is_transient = function(resp) {
          httr2::resp_status(resp) == 429 || httr2::resp_status(resp) >= 500
        }
      )
      req <- httr2::req_error(req, is_error = function(resp) FALSE)

      resp <- tryCatch(
        httr2::req_perform(req),
        httr2_error = function(e) {
          stop_openalex(paste("OpenAlex request failed:",
                              conditionMessage(e)))
        }
      )

      status <- httr2::resp_status(resp)
      if (status == 404) return(list())
      if (status >= 400) {
        body <- tryCatch(httr2::resp_body_string(resp),
                         error = function(e) "")
        if (is.null(body) || is.na(body)) body <- ""
        stop_openalex(
          paste0("OpenAlex HTTP ", status, " for ", path, ": ",
                 substr(body, 1, 300)),
          status_code = status,
          response_text = body
        )
      }
      httr2::resp_body_json(resp)
    },

    get_works_by_doi_values = function(values) {
      if (length(values) == 0L) return(list())
      tryCatch({
        data <- private$request_json("/works", list(
          filter = paste0("doi:", paste(values, collapse = "|")),
          `per-page` = max(1L, min(100L, length(values))),
          select = RESOLUTION_SELECT
        ))
        data$results %||% list()
      }, OpenAlexError = function(e) {
        if (is.na(e$status_code) || e$status_code != 400) stop(e)
        if (length(values) == 1L) return(list())
        mid <- length(values) %/% 2L
        c(private$get_works_by_doi_values(values[seq_len(mid)]),
          private$get_works_by_doi_values(values[(mid + 1L):length(values)]))
      })
    }
  )
)

#' Normalize a raw OpenAlex work record into the storage schema.
#'
#' @param work list, a raw OpenAlex work record.
#' @return named list with `WORK_COLUMNS`.
normalize_work <- function(work) {
  if (is.null(work)) work <- list()
  openalex_id <- compact_openalex_id(work$id)
  primary_location <- work$primary_location %||% list()
  source <- primary_location$source %||% list()
  primary_topic <- work$primary_topic %||% list()
  topic_domain <- primary_topic$domain %||% list()
  referenced <- vapply(work$referenced_works %||% list(),
                      compact_openalex_id, character(1))
  referenced <- referenced[!is.na(referenced) & referenced != ""]
  list(
    openalex_id = openalex_id,
    doi = clean_doi(work$doi),
    title = work$display_name %||% work$title,
    publication_date = work$publication_date,
    publication_year = work$publication_year,
    work_type = work$type %||% work$type_crossref,
    is_retracted = work$is_retracted,
    cited_by_count = work$cited_by_count,
    source_id = compact_openalex_id(source$id),
    source_name = source$display_name %||% NA_character_,
    topic_id = compact_openalex_id(primary_topic$id),
    topic_name = primary_topic$display_name %||% NA_character_,
    topic_domain = topic_domain$display_name %||% NA_character_,
    referenced_works_json = json_dumps(referenced),
    raw_json = json_dumps(work)
  )
}

#' Extract citation edges from a work's `referenced_works`, intersected
#' with the set of parent IDs we are currently crawling from.
#'
#' @param work list, a raw OpenAlex work record.
#' @param parent_ids character vector of compact parent OpenAlex IDs.
#' @return list of `c(source, target)` character pairs.
edges_from_work <- function(work, parent_ids) {
  source_id <- compact_openalex_id(work$id)
  if (is.na(source_id) || source_id == "") return(list())
  referenced <- vapply(work$referenced_works %||% list(),
                     compact_openalex_id, character(1))
  referenced <- referenced[!is.na(referenced) & referenced != ""]
  parents <- vapply(parent_ids, compact_openalex_id, character(1))
  parents <- parents[!is.na(parents) & parents != ""]
  hits <- intersect(referenced, parents)
  if (length(hits) == 0L) return(list())
  hits <- sort(unique(hits))
  lapply(hits, function(t) c(source_id, t))
}

#' Build the full OpenAlex URL for a compact ID.
#' @param openalex_id character compact OpenAlex ID or `NA`.
#' @return full URL or `NA_character_`.
openalex_url <- function(openalex_id) {
  full_openalex_id(openalex_id)
}