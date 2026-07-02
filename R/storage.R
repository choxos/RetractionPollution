#' @title DuckDB storage layer for retractionpollution
#' @description R6 class `StudyStore` wrapping a DuckDB file with the
#'   citation-pollution schema (seeds, works, frontier_nodes, citation_edges,
#'   crawl_jobs, run_metadata). Ported from the original Python pipeline
#'   (`retraction_pollution.storage`) with the HIGH-2 audit fix: edges use a
#'   min-depth merge instead of `INSERT OR IGNORE` so an edge always carries
#'   the shallowest depth at which it was observed.

library(R6)
library(DBI)
library(duckdb)

STORAGE_TABLES <- c(
  "seeds", "works", "frontier_nodes", "citation_edges",
  "crawl_jobs", "run_metadata"
)

SEED_COLUMNS <- c(
  "record_id", "title", "notice_type", "notice_date", "original_paper_date",
  "original_doi", "original_pmid", "author", "journal", "publisher",
  "subject", "reason", "article_type", "country", "openalex_id",
  "resolved_by", "resolved_status", "source_row_json"
)

WORK_COLUMNS <- c(
  "openalex_id", "doi", "title", "publication_date", "publication_year",
  "work_type", "is_retracted", "cited_by_count", "source_id", "source_name",
  "topic_id", "topic_name", "topic_domain", "referenced_works_json", "raw_json"
)

#' @title DuckDB study store
#' @description R6 class managing the citation-pollution DuckDB database.
#' @noRd
StudyStore <- R6::R6Class(
  "StudyStore",
  cloneable = FALSE,
  public = list(
    db_path = NULL,

    initialize = function(db_path) {
      self$db_path <- db_path
      ensure_dir(dirname(db_path))
      private$drv <- duckdb::duckdb()
      private$gconn <- DBI::dbConnect(private$drv, db_path)
      self$init_schema()
    },

    close = function() {
      if (!DBI::dbIsValid(private$gconn)) return(invisible(NULL))
      DBI::dbDisconnect(private$gconn)
      invisible(NULL)
    },

    init_schema = function() {
      con <- private$gconn
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS seeds (
            record_id TEXT PRIMARY KEY,
            title TEXT, notice_type TEXT, notice_date DATE,
            original_paper_date DATE, original_doi TEXT, original_pmid TEXT,
            author TEXT, journal TEXT, publisher TEXT, subject TEXT,
            reason TEXT, article_type TEXT, country TEXT, openalex_id TEXT,
            resolved_by TEXT, resolved_status TEXT, source_row_json TEXT,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS works (
            openalex_id TEXT PRIMARY KEY, doi TEXT, title TEXT,
            publication_date DATE, publication_year INTEGER, work_type TEXT,
            is_retracted BOOLEAN, cited_by_count INTEGER, source_id TEXT,
            source_name TEXT, topic_id TEXT, topic_name TEXT,
            topic_domain TEXT, referenced_works_json TEXT, raw_json TEXT,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS frontier_nodes (
            openalex_id TEXT PRIMARY KEY, depth INTEGER NOT NULL,
            processed_at TIMESTAMP, added_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS citation_edges (
            source_id TEXT NOT NULL, target_id TEXT NOT NULL,
            depth INTEGER NOT NULL,
            source_api TEXT NOT NULL DEFAULT 'openalex',
            citation_date DATE,
            discovered_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY (source_id, target_id)
        )
      ")
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS crawl_jobs (
            job_id TEXT PRIMARY KEY, depth INTEGER NOT NULL,
            parent_ids_json TEXT NOT NULL, cursor TEXT NOT NULL,
            done BOOLEAN NOT NULL DEFAULT FALSE,
            pages_fetched INTEGER NOT NULL DEFAULT 0,
            results_fetched INTEGER NOT NULL DEFAULT 0,
            error TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
      DBI::dbExecute(con, "
        CREATE TABLE IF NOT EXISTS run_metadata (
            key TEXT PRIMARY KEY, value TEXT,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
      invisible(NULL)
    },

    upsert_seed = function(seed) {
      cols <- SEED_COLUMNS
      values <- lapply(cols, function(col) {
        v <- seed[[col]]
        if (is.null(v)) NA else v
      })
      placeholders <- paste(rep("?", length(cols)), collapse = ", ")
      update_cols <- cols[-1]
      update_clause <- paste(vapply(update_cols, function(col) {
        sprintf("%s = excluded.%s", col, col)
      }, character(1)), collapse = ", ")
      sql <- sprintf(
        "INSERT INTO seeds (%s) VALUES (%s)
         ON CONFLICT(record_id) DO UPDATE SET %s, updated_at = now()",
        paste(cols, collapse = ", "),
        placeholders,
        update_clause
      )
      DBI::dbExecute(private$gconn, sql, params = as.list(values))
      oa <- seed$openalex_id
      if (!is_missing(oa)) {
        self$add_frontier_node(oa, 0L)
      }
      invisible(NULL)
    },

    upsert_seeds = function(seeds) {
      count <- 0L
      for (seed in seeds) {
        self$upsert_seed(seed)
        count <- count + 1L
      }
      count
    },

    update_seed_resolution = function(record_id, openalex_id, resolved_by,
                                       status) {
      DBI::dbExecute(
        private$gconn,
        "UPDATE seeds
         SET openalex_id = ?, resolved_by = ?, resolved_status = ?,
             updated_at = now()
         WHERE record_id = ?",
        params = list(openalex_id, resolved_by, status, record_id)
      )
      if (!is_missing(openalex_id)) {
        self$add_frontier_node(openalex_id, 0L)
      }
      invisible(NULL)
    },

    unresolved_seeds = function(limit = NA, include_pending_title_fallback = TRUE) {
      sql <- "
        SELECT *
        FROM seeds
        WHERE openalex_id IS NULL
          AND (resolved_status IS NULL OR resolved_status != 'not_found')
      "
      if (!isTRUE(include_pending_title_fallback)) {
        sql <- paste(sql, "
          AND (resolved_status IS NULL
               OR resolved_status != 'pending_title_fallback')
        ")
      }
      sql <- paste(sql, "ORDER BY record_id")
      if (!is_missing(limit)) {
        sql <- paste0(sql, " LIMIT ", as.integer(limit))
      }
      df <- DBI::dbGetQuery(private$gconn, sql)
      df_to_records(df)
    },

    resolved_seed_ids = function() {
      df <- DBI::dbGetQuery(private$gconn, "
        SELECT DISTINCT openalex_id
        FROM seeds
        WHERE openalex_id IS NOT NULL
        ORDER BY openalex_id
      ")
      as.character(df$openalex_id)
    },

    resolved_seeds_with_doi = function() {
      df <- DBI::dbGetQuery(private$gconn, "
        SELECT record_id, title, original_doi, original_pmid,
               openalex_id, notice_date
        FROM seeds
        WHERE openalex_id IS NOT NULL AND original_doi IS NOT NULL
        ORDER BY record_id
      ")
      df_to_records(df)
    },

    frontier_with_doi = function(depth) {
      df <- DBI::dbGetQuery(
        private$gconn,
        "
        SELECT
            f.openalex_id,
            COALESCE(w.doi, s.original_doi) AS doi
        FROM frontier_nodes f
        LEFT JOIN works w ON f.openalex_id = w.openalex_id
        LEFT JOIN seeds s ON f.openalex_id = s.openalex_id
        WHERE f.depth = ?
          AND COALESCE(w.doi, s.original_doi) IS NOT NULL
          AND f.processed_at IS NULL
        ORDER BY f.openalex_id
        ",
        params = list(as.integer(depth))
      )
      df_to_records(df)
    },

    upsert_work = function(work) {
      cols <- WORK_COLUMNS
      values <- lapply(cols, function(col) {
        v <- work[[col]]
        if (is.null(v)) NA else v
      })
      placeholders <- paste(rep("?", length(cols)), collapse = ", ")
      update_cols <- cols[-1]
      update_clause <- paste(vapply(update_cols, function(col) {
        sprintf("%s = excluded.%s", col, col)
      }, character(1)), collapse = ", ")
      sql <- sprintf(
        "INSERT INTO works (%s) VALUES (%s)
         ON CONFLICT(openalex_id) DO UPDATE SET %s, updated_at = now()",
        paste(cols, collapse = ", "),
        placeholders,
        update_clause
      )
      DBI::dbExecute(private$gconn, sql, params = as.list(values))
      invisible(NULL)
    },

    add_frontier_node = function(openalex_id, depth) {
      existing <- DBI::dbGetQuery(
        private$gconn,
        "SELECT depth FROM frontier_nodes WHERE openalex_id = ?",
        params = list(openalex_id)
      )
      if (nrow(existing) == 0L) {
        DBI::dbExecute(
          private$gconn,
          "INSERT INTO frontier_nodes (openalex_id, depth) VALUES (?, ?)",
          params = list(openalex_id, as.integer(depth))
        )
      } else if (as.integer(depth) < as.integer(existing$depth[1])) {
        DBI::dbExecute(
          private$gconn,
          "UPDATE frontier_nodes SET depth = ? WHERE openalex_id = ?",
          params = list(as.integer(depth), openalex_id)
        )
      }
      invisible(NULL)
    },

    add_edge = function(source_id, target_id, depth,
                        source_api = "openalex", citation_date = NA) {
      cd <- if (is_missing(citation_date)) NA else citation_date
      DBI::dbExecute(
        private$gconn,
        "
        INSERT INTO citation_edges
            (source_id, target_id, depth, source_api, citation_date)
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(source_id, target_id) DO UPDATE SET
            depth = CASE WHEN excluded.depth < citation_edges.depth
                         THEN excluded.depth ELSE citation_edges.depth END,
            source_api = CASE WHEN excluded.depth < citation_edges.depth
                              THEN excluded.source_api
                              ELSE citation_edges.source_api END,
            citation_date = CASE WHEN excluded.depth < citation_edges.depth
                                 THEN excluded.citation_date
                                 ELSE citation_edges.citation_date END
        ",
        params = list(
          source_id, target_id, as.integer(depth), source_api, cd
        )
      )
      invisible(NULL)
    },

    pending_frontier = function(depth, limit = NA) {
      sql <- "
        SELECT openalex_id
        FROM frontier_nodes
        WHERE depth = ? AND processed_at IS NULL
        ORDER BY openalex_id
      "
      if (!is_missing(limit)) {
        sql <- paste0(sql, " LIMIT ", as.integer(limit))
      }
      df <- DBI::dbGetQuery(private$gconn, sql,
                            params = list(as.integer(depth)))
      as.character(df$openalex_id)
    },

    mark_processed = function(parent_ids) {
      for (openalex_id in parent_ids) {
        DBI::dbExecute(
          private$gconn,
          "UPDATE frontier_nodes SET processed_at = now() WHERE openalex_id = ?",
          params = list(openalex_id)
        )
      }
      invisible(NULL)
    },

    get_or_create_job = function(job_id, depth, parent_ids) {
      existing <- DBI::dbGetQuery(
        private$gconn,
        "SELECT job_id FROM crawl_jobs WHERE job_id = ?",
        params = list(job_id)
      )
      if (nrow(existing) == 0L) {
        DBI::dbExecute(
          private$gconn,
          "INSERT INTO crawl_jobs (job_id, depth, parent_ids_json, cursor)
           VALUES (?, ?, ?, '*')",
          params = list(job_id, as.integer(depth), json_dumps(parent_ids))
        )
      }
      self$get_job(job_id)
    },

    get_job = function(job_id) {
      df <- DBI::dbGetQuery(
        private$gconn,
        "SELECT * FROM crawl_jobs WHERE job_id = ?",
        params = list(job_id)
      )
      if (nrow(df) == 0L) {
        stop("job not found: ", job_id, call. = FALSE)
      }
      df_to_records(df)[[1]]
    },

    update_job = function(job_id, cursor = NA, done = NA, pages_delta = 0L,
                          results_delta = 0L, error = NA) {
      job <- self$get_job(job_id)
      new_cursor <- if (is_missing(cursor)) job$cursor else cursor
      new_done <- if (is_missing(done)) job$done else done
      new_pages <- as.integer(job$pages_fetched) + as.integer(pages_delta)
      new_results <- as.integer(job$results_fetched) + as.integer(results_delta)
      new_error <- if (is_missing(error)) NA_character_ else error
      DBI::dbExecute(
        private$gconn,
        "UPDATE crawl_jobs
         SET cursor = ?, done = ?, pages_fetched = ?, results_fetched = ?,
             error = ?, updated_at = now()
         WHERE job_id = ?",
        params = list(
          new_cursor, new_done, new_pages, new_results, new_error, job_id
        )
      )
      invisible(NULL)
    },

    set_metadata = function(key, value) {
      stored <- if (is.character(value) && length(value) == 1L) {
        value
      } else {
        json_dumps(value)
      }
      DBI::dbExecute(
        private$gconn,
        "INSERT INTO run_metadata (key, value)
         VALUES (?, ?)
         ON CONFLICT(key) DO UPDATE SET
             value = excluded.value, updated_at = now()",
        params = list(key, stored)
      )
      invisible(NULL)
    },

    get_metadata = function(key, default = NA) {
      df <- DBI::dbGetQuery(
        private$gconn,
        "SELECT value FROM run_metadata WHERE key = ?",
        params = list(key)
      )
      if (nrow(df) == 0L) return(default)
      as.character(df$value[1])
    },

    delete_metadata = function(key) {
      DBI::dbExecute(
        private$gconn,
        "DELETE FROM run_metadata WHERE key = ?",
        params = list(key)
      )
      invisible(NULL)
    },

    count_frontier_depth = function(depth) {
      df <- DBI::dbGetQuery(
        private$gconn,
        "SELECT COUNT(*) AS n FROM frontier_nodes WHERE depth = ?",
        params = list(as.integer(depth))
      )
      as.integer(df$n[1])
    },

    export_parquet_tables = function(out_dir) {
      ensure_dir(out_dir)
      for (table in STORAGE_TABLES) {
        path <- file.path(out_dir, paste0(table, ".parquet"))
        sql <- sprintf(
          "COPY (SELECT * FROM %s) TO '%s' (FORMAT PARQUET)",
          table, path
        )
        DBI::dbExecute(private$gconn, sql)
      }
      invisible(NULL)
    }
  ),

  active = list(
    con = function() private$gconn
  ),

  private = list(
    gconn = NULL,
    drv = NULL,

    finalize = function() {
      tryCatch(self$close(), error = function(e) NULL)
    }
  )
)

#' Convert a data frame into a list of named lists (one per row).
#' @param df data.frame
#' @return list of named lists.
df_to_records <- function(df) {
  if (nrow(df) == 0L) return(list())
  out <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    row <- as.list(df[i, , drop = FALSE])
    out[[i]] <- row
  }
  out
}
