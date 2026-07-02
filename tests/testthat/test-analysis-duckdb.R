library(testthat)

# Source the modules under test (mirrors test-analysis.R).
for (mod in c("util.R", "storage.R", "seed_units.R", "analysis.R",
              "manuscript.R")) {
  p <- file.path(testthat::test_path("..", "..", "R", mod))
  if (file.exists(p)) source(p)
}

# Distinct IDs so this file's fixture never collides with test-analysis.R.
QS1 <- "W8000000001"
QS2 <- "W8000000002"
QD1A <- "W8000000003"
QD1B <- "W8000000004"
QD1C <- "W8000000005"
QD2A <- "W8000000006"
QD2B <- "W8000000007"

# Same topology as test-analysis.R:
#   QS1 (depth 0) <- QD1A (post-notice), QD1B (pre-notice)
#   QS2 (depth 0) <- QD1C (post-notice)
#   QD1A <- QD2A (depth 2); QD1C <- QD2B (depth 2)
setup_duckdb_store <- function(env = parent.frame()) {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- StudyStore$new(db)
  withr::defer(store$close(), envir = env)

  store$upsert_seed(list(
    record_id = "QR1", title = "Seed One", notice_type = "Retraction",
    notice_date = as.Date("2020-01-15"),
    original_paper_date = as.Date("2019-01-01"),
    original_doi = "10.1000/qseed1", original_pmid = NA,
    author = "A", journal = "Nature", publisher = "Springer",
    subject = "Bio", reason = "fraud", article_type = "Article",
    country = "US", openalex_id = QS1,
    resolved_by = "test", resolved_status = "resolved", source_row_json = NA
  ))
  store$upsert_seed(list(
    record_id = "QR2", title = "Seed Two", notice_type = "Retraction",
    notice_date = as.Date("2019-06-01"),
    original_paper_date = as.Date("2018-01-01"),
    original_doi = "10.1000/qseed2", original_pmid = NA,
    author = "B", journal = "Science", publisher = "Elsevier",
    subject = "Phys", reason = "error", article_type = "Article",
    country = "UK", openalex_id = QS2,
    resolved_by = "test", resolved_status = "resolved", source_row_json = NA
  ))

  add_work <- function(oa, pub_date, pub_year, is_retracted = FALSE) {
    store$upsert_work(list(
      openalex_id = oa, doi = NA, title = paste("Work", oa),
      publication_date = as.Date(pub_date), publication_year = pub_year,
      work_type = "Article", is_retracted = is_retracted, cited_by_count = 0L,
      source_id = NA, source_name = "Src", topic_id = NA,
      topic_name = "Topic", topic_domain = "Domain",
      referenced_works_json = NA, raw_json = NA
    ))
  }
  add_work(QS1, "2019-01-01", 2019L, TRUE)
  add_work(QS2, "2018-01-01", 2018L, TRUE)
  add_work(QD1A, "2021-03-10", 2021L)
  add_work(QD1B, "2018-05-01", 2018L)
  add_work(QD1C, "2020-08-20", 2020L)
  add_work(QD2A, "2022-04-01", 2022L)
  add_work(QD2B, "2021-12-15", 2021L)

  for (id_depth in list(c(QS1, 0), c(QS2, 0), c(QD1A, 1), c(QD1B, 1),
                        c(QD1C, 1), c(QD2A, 2), c(QD2B, 2))) {
    store$add_frontier_node(id_depth[[1]], as.integer(id_depth[[2]]))
  }
  store$add_edge(QD1A, QS1, 1L, source_api = "opencitations")
  store$add_edge(QD1B, QS1, 1L, source_api = "opencitations")
  store$add_edge(QD1C, QS2, 1L, source_api = "opencitations")
  store$add_edge(QD2A, QD1A, 2L, source_api = "opencitations")
  store$add_edge(QD2B, QD1C, 2L, source_api = "opencitations")
  store
}

in_memory_seed_metrics <- function(store, max_depth = 2L) {
  nodes <- tibble::as_tibble(DBI::dbGetQuery(store$con,
    "SELECT f.openalex_id, f.depth, w.doi, w.title, w.publication_date,
            w.publication_year, w.work_type, w.is_retracted, w.cited_by_count,
            w.source_name, w.topic_name, w.topic_domain
     FROM frontier_nodes f LEFT JOIN works w ON f.openalex_id = w.openalex_id
     WHERE f.depth <= ?", params = list(as.integer(max_depth))))
  edges <- tibble::as_tibble(DBI::dbGetQuery(store$con,
    "SELECT e.source_id, e.target_id, e.depth, e.source_api, e.citation_date
     FROM citation_edges e
     JOIN frontier_nodes fs ON e.source_id = fs.openalex_id
     JOIN frontier_nodes ft ON e.target_id = ft.openalex_id
     WHERE fs.depth <= ? AND ft.depth <= ?",
    params = list(as.integer(max_depth), as.integer(max_depth))))
  seeds <- tibble::as_tibble(DBI::dbGetQuery(store$con, "SELECT * FROM seeds"))
  seed_metrics(seeds, nodes, edges)
}

test_that("DuckDB analysis path matches the in-memory path", {
  store <- setup_duckdb_store()
  expected <- in_memory_seed_metrics(store)

  out <- withr::local_tempdir()
  withr::local_options(retractionpollution.analysis_in_memory_edge_limit = 0)
  summary <- run_analysis(store, out, max_analysis_depth = 2L)

  # It actually took the large/DuckDB branch (streams gzip GraphML + manifest).
  expect_true(file.exists(file.path(out, "graphs",
                                    "network_depth2.graphml.gz")))
  expect_true(file.exists(file.path(out, "graphs",
                                    "network_depth2.graphml.json")))
  expect_equal(summary$weak_components,
               "not_computed_large_graph_scale_limit")

  got <- readr::read_csv(file.path(out, "tables", "top_polluted_seeds.csv"),
                         show_col_types = FALSE)
  key <- c("direct_citers", "depth2_descendants", "total_depth2_reach",
           "post_notice_direct_citers", "post_notice_depth2_descendants")
  e <- expected[order(expected$openalex_id), c("openalex_id", key)]
  g <- got[order(got$openalex_id), c("openalex_id", key)]
  expect_equal(as.data.frame(g[, key]), as.data.frame(e[, key]))

  # Summary edge/node counts agree with the toy graph.
  expect_equal(summary$depth2_edges, 5L)
  expect_equal(summary$stored_edges, 5L)
  expect_equal(summary$depth0_nodes, 2L)
  expect_equal(summary$depth1_nodes, 3L)
  expect_equal(summary$depth2_nodes_only, 2L)
})

test_that("build_manuscript reproduces headline numbers on a toy graph", {
  store <- setup_duckdb_store()
  out <- withr::local_tempdir()
  withr::local_options(retractionpollution.analysis_in_memory_edge_limit = 0)
  run_analysis(store, out, max_analysis_depth = 2L)

  stats_path <- build_manuscript(store, out, max_analysis_depth = 2L,
                                 render = FALSE, compute_unique_post_d2 = TRUE)
  expect_true(file.exists(stats_path))

  st <- jsonlite::fromJSON(stats_path)
  expect_equal(st$total_edges, 5)
  expect_equal(st$depth1_edges, 3)
  expect_equal(st$depth2_edges, 2)
  expect_equal(st$total_nodes, 7)

  # Unique post-notice direct nodes: QD1A (post S1) and QD1C (post S2) = 2.
  expect_equal(st$post_notice$direct_unique_nodes, 2)
  # Per-seed relationship sum is also 2 here (no shared citers).
  expect_equal(st$post_notice$direct_relationship_sum, 2)
  # Unique post-notice depth-2 nodes: QD2A and QD2B = 2.
  expect_equal(st$post_notice$depth2_unique_nodes, 2)

  figs <- c("manuscript_fig1_depth_counts",
            "manuscript_fig2_reach_survival_concentration",
            "manuscript_fig3_edge_depths",
            "manuscript_fig4_reach_distribution",
            "manuscript_fig5_top_seed_reach",
            "manuscript_fig6_post_notice_eventstudy")
  for (fig in figs) {
    expect_true(file.exists(file.path(out, "figures", paste0(fig, ".png"))),
                info = paste("missing figure:", fig))
  }
})
