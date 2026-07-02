#' @title Seed record canonicalization helpers
#' @description Helpers for collapsing Retraction Watch records to unique
#'   source papers. Retraction Watch can contain multiple records for the same
#'   DOI, for example an expression of concern followed by a retraction.

#' Return the first non-missing scalar value.
#' @param values vector
#' @return scalar value or `NA_character_`.
first_non_missing <- function(values) {
  values <- values[!is.na(values) & as.character(values) != ""]
  if (length(values) == 0L) return(NA_character_)
  values[[1]]
}

#' Return the minimum Date or `NA`.
#' @param values vector coercible to Date.
#' @return Date.
min_date_or_na <- function(values) {
  dates <- as.Date(values)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0L) return(as.Date(NA))
  min(dates)
}

#' Collapse resolved seed records to one row per resolved source paper.
#'
#' The canonical notice date is the earliest available notice date for the
#' resolved paper, so post-notice metrics count citations after the first public
#' warning or retraction notice represented in Retraction Watch.
#'
#' @param seeds data.frame with seed columns.
#' @return tibble, one row per `openalex_id`.
canonical_seed_records <- function(seeds) {
  empty <- tibble::tibble(
    record_id = character(),
    seed_record_count = integer(),
    seed_record_ids = character(),
    openalex_id = character(),
    title = character(),
    notice_type = character(),
    notice_date = as.Date(character())
  )
  if (nrow(seeds) == 0L || !"openalex_id" %in% names(seeds)) return(empty)

  seeds |>
    dplyr::filter(!is.na(openalex_id)) |>
    dplyr::mutate(
      notice_date = as.Date(notice_date),
      record_id = as.character(record_id)
    ) |>
    dplyr::arrange(openalex_id, notice_date, record_id) |>
    dplyr::group_by(openalex_id) |>
    dplyr::summarise(
      seed_record_count = dplyr::n_distinct(record_id),
      seed_record_ids = paste(unique(record_id), collapse = ";"),
      record_id = first_non_missing(record_id),
      title = first_non_missing(title),
      notice_type = paste(unique(stats::na.omit(notice_type)), collapse = "; "),
      notice_date = min_date_or_na(notice_date),
      .groups = "drop"
    ) |>
    dplyr::select(record_id, seed_record_count, seed_record_ids,
                  openalex_id, title, notice_type, notice_date)
}

#' Count duplicate resolved seed records.
#' @param seeds data.frame with seed columns.
#' @return named integer list.
seed_duplicate_counts <- function(seeds) {
  if (nrow(seeds) == 0L || !"openalex_id" %in% names(seeds)) {
    return(list(
      resolved_seed_papers = 0L,
      duplicate_resolved_seed_records = 0L,
      duplicate_seed_rows_in_duplicate_groups = 0L
    ))
  }
  resolved <- seeds |> dplyr::filter(!is.na(openalex_id))
  if (nrow(resolved) == 0L) {
    return(list(
      resolved_seed_papers = 0L,
      duplicate_resolved_seed_records = 0L,
      duplicate_seed_rows_in_duplicate_groups = 0L
    ))
  }
  by_id <- resolved |> dplyr::count(openalex_id, name = "n")
  papers <- nrow(by_id)
  duplicate_groups <- by_id |> dplyr::filter(n > 1L)
  list(
    resolved_seed_papers = as.integer(papers),
    duplicate_resolved_seed_records = as.integer(nrow(resolved) - papers),
    duplicate_seed_rows_in_duplicate_groups = as.integer(sum(duplicate_groups$n))
  )
}

#' Split a list of seed records into canonical DOI seeds and duplicates.
#' @param seeds list of seed records.
#' @return list with `canonical`, `duplicates`, and `no_doi` seed lists.
partition_seed_list_by_doi <- function(seeds) {
  if (length(seeds) > 1L) {
    doi_keys <- vapply(seeds, function(seed) {
      doi <- clean_doi(seed$original_doi)
      if (is.na(doi)) "" else doi
    }, character(1))
    notice_dates <- vapply(seeds, function(seed) {
      date <- parse_date(seed$notice_date)
      if (is.na(date)) "9999-12-31" else date
    }, character(1))
    record_ids <- vapply(seeds, function(seed) {
      id <- seed$record_id
      if (is_missing(id)) "" else as.character(id)
    }, character(1))
    seeds <- seeds[order(doi_keys, notice_dates, record_ids)]
  }

  seen <- new.env(parent = emptyenv())
  canonical <- list()
  duplicates <- list()
  no_doi <- list()

  for (seed in seeds) {
    doi <- clean_doi(seed$original_doi)
    if (is.na(doi)) {
      no_doi[[length(no_doi) + 1L]] <- seed
      next
    }
    if (exists(doi, envir = seen, inherits = FALSE)) {
      duplicates[[length(duplicates) + 1L]] <- seed
      next
    }
    assign(doi, TRUE, envir = seen)
    canonical[[length(canonical) + 1L]] <- seed
  }

  list(canonical = canonical, duplicates = duplicates, no_doi = no_doi)
}

#' Mark duplicate linked seed records in an existing store.
#'
#' Keeps the earliest notice row per linked paper as `resolved` and marks later
#' rows for the same `openalex_id` as `duplicate_doi`. The duplicate rows keep
#' their `openalex_id` so provenance and multi-notice summaries remain possible.
#'
#' @param store StudyStore.
#' @return number of records marked `duplicate_doi`.
mark_duplicate_seed_resolutions <- function(store) {
  DBI::dbExecute(store$con, "
    WITH ranked AS (
      SELECT
        record_id,
        ROW_NUMBER() OVER (
          PARTITION BY openalex_id
          ORDER BY notice_date NULLS LAST, record_id
        ) AS rn
      FROM seeds
      WHERE openalex_id IS NOT NULL
    )
    UPDATE seeds
    SET
      resolved_status = CASE
        WHEN ranked.rn = 1 THEN 'resolved'
        ELSE 'duplicate_doi'
      END,
      resolved_by = COALESCE(resolved_by, 'opencitations_doi'),
      updated_at = now()
    FROM ranked
    WHERE seeds.record_id = ranked.record_id
  ")
  df <- DBI::dbGetQuery(store$con, "
    SELECT COUNT(*) AS n
    FROM seeds
    WHERE resolved_status = 'duplicate_doi'
  ")
  as.integer(df$n[1])
}
