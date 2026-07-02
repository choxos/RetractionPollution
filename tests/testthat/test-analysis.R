library(testthat)

# Source the modules under test.
util_path <- file.path(testthat::test_path("..", "..", "R", "util.R"))
if (file.exists(util_path)) source(util_path)
storage_path <- file.path(testthat::test_path("..", "..", "R", "storage.R"))
if (file.exists(storage_path)) source(storage_path)
seed_units_path <- file.path(testthat::test_path("..", "..", "R", "seed_units.R"))
if (file.exists(seed_units_path)) source(seed_units_path)
analysis_path <- file.path(testthat::test_path("..", "..", "R", "analysis.R"))
if (file.exists(analysis_path)) source(analysis_path)

# IDs
S1 <- "W1111111111"
S2 <- "W2222222222"
D1A <- "W3333333333"
D1B <- "W4444444444"
D1C <- "W5555555555"
D2A <- "W6666666666"
D2B <- "W7777777777"

#' Build a StudyStore populated with a small citation-pollution graph.
#'
#' Topology:
#'   seed1 (S1, depth 0) <- cited by D1A (after notice), D1B (before notice)
#'   seed2 (S2, depth 0) <- cited by D1C (after notice)
#'   D1A <- cited by D2A (depth 2)
#'   D1C <- cited by D2B (depth 2)
setup_test_store <- function(env = parent.frame()) {
  db <- withr::local_tempfile(fileext = ".duckdb")
  store <- StudyStore$new(db)
  withr::defer(store$close(), envir = env)

  store$upsert_seed(list(
    record_id = "R1", title = "Seed One", notice_type = "Retraction",
    notice_date = as.Date("2020-01-15"),
    original_paper_date = as.Date("2019-01-01"),
    original_doi = "10.1000/seed1", original_pmid = NA,
    author = "A. Author", journal = "Nature", publisher = "Springer",
    subject = "Biology", reason = "fraud", article_type = "Article",
    country = "US", openalex_id = S1,
    resolved_by = "test", resolved_status = "resolved", source_row_json = NA
  ))
  store$upsert_seed(list(
    record_id = "R2", title = "Seed Two", notice_type = "Retraction",
    notice_date = as.Date("2019-06-01"),
    original_paper_date = as.Date("2018-01-01"),
    original_doi = "10.1000/seed2", original_pmid = NA,
    author = "B. Author", journal = "Science", publisher = "Elsevier",
    subject = "Physics", reason = "error", article_type = "Article",
    country = "UK", openalex_id = S2,
    resolved_by = "test", resolved_status = "resolved", source_row_json = NA
  ))

  add_work <- function(oa, title, pub_date, pub_year, topic_name = "Topic",
                       topic_domain = "Domain", is_retracted = FALSE,
                       work_type = "Article", source_name = "Src") {
    store$upsert_work(list(
      openalex_id = oa, doi = NA, title = title,
      publication_date = as.Date(pub_date), publication_year = pub_year,
      work_type = work_type, is_retracted = is_retracted,
      cited_by_count = 0L, source_id = NA, source_name = source_name,
      topic_id = NA, topic_name = topic_name, topic_domain = topic_domain,
      referenced_works_json = NA, raw_json = NA
    ))
  }

  add_work(S1, "Seed One", "2019-01-01", 2019L, is_retracted = TRUE)
  add_work(S2, "Seed Two", "2018-01-01", 2018L, is_retracted = TRUE)
  # D1A published AFTER seed1 notice (2020-01-15) -> post-notice citer
  add_work(D1A, "Depth1 A", "2021-03-10", 2021L)
  # D1B published BEFORE seed1 notice -> not post-notice
  add_work(D1B, "Depth1 B", "2018-05-01", 2018L)
  # D1C published AFTER seed2 notice (2019-06-01)
  add_work(D1C, "Depth1 C", "2020-08-20", 2020L)
  add_work(D2A, "Depth2 A", "2022-04-01", 2022L)
  add_work(D2B, "Depth2 B", "2021-12-15", 2021L)

  store$add_frontier_node(S1, 0L)
  store$add_frontier_node(S2, 0L)
  store$add_frontier_node(D1A, 1L)
  store$add_frontier_node(D1B, 1L)
  store$add_frontier_node(D1C, 1L)
  store$add_frontier_node(D2A, 2L)
  store$add_frontier_node(D2B, 2L)

  # Edges: source cites target. depth = depth of source.
  store$add_edge(D1A, S1, 1L)
  store$add_edge(D1B, S1, 1L)
  store$add_edge(D1C, S2, 1L)
  store$add_edge(D2A, D1A, 2L)
  store$add_edge(D2B, D1C, 2L)

  store
}

load_test_frame <- function(store, max_depth = 2L) {
  nodes <- tibble::as_tibble(DBI::dbGetQuery(
    store$con,
    "SELECT f.openalex_id, f.depth, w.doi, w.title, w.publication_date,
            w.publication_year, w.work_type, w.is_retracted, w.cited_by_count,
            w.source_name, w.topic_name, w.topic_domain
     FROM frontier_nodes f
     LEFT JOIN works w ON f.openalex_id = w.openalex_id
     WHERE f.depth <= ?",
    params = list(as.integer(max_depth))
  ))
  edges <- tibble::as_tibble(DBI::dbGetQuery(
    store$con,
    "SELECT e.source_id, e.target_id, e.depth, e.source_api, e.citation_date
     FROM citation_edges e
     JOIN frontier_nodes fs ON e.source_id = fs.openalex_id
     JOIN frontier_nodes ft ON e.target_id = ft.openalex_id
     WHERE fs.depth <= ? AND ft.depth <= ?",
    params = list(as.integer(max_depth), as.integer(max_depth))
  ))
  seeds <- tibble::as_tibble(DBI::dbGetQuery(store$con, "SELECT * FROM seeds"))
  list(nodes = nodes, edges = edges, seeds = seeds)
}

test_that("seed_metrics returns correct counts (CRITICAL-1 regression)", {
  store <- setup_test_store()
  on.exit(store$close(), add = TRUE)
  f <- load_test_frame(store)
  m <- seed_metrics(f$seeds, f$nodes, f$edges)

  expect_s3_class(m, "data.frame")
  expect_true(all(c("record_id", "openalex_id", "direct_citers",
                    "depth2_descendants", "total_depth2_reach",
                    "post_notice_direct_citers",
                    "post_notice_depth2_descendants") %in% names(m)))

  # Seed1: direct citers D1A + D1B = 2; depth2 descendant D2A = 1; union = 3
  m1 <- m[m$openalex_id == S1, ]
  expect_equal(m1$direct_citers, 2L)
  expect_equal(m1$depth2_descendants, 1L)
  expect_equal(m1$total_depth2_reach, 3L)
  # Only D1A published after notice (2020-01-15). D1B is before.
  expect_equal(m1$post_notice_direct_citers, 1L)
  # D2A published 2022 > notice -> post-notice depth2
  expect_equal(m1$post_notice_depth2_descendants, 1L)

  # Seed2: direct citer D1C = 1; depth2 descendant D2B = 1; union = 2
  m2 <- m[m$openalex_id == S2, ]
  expect_equal(m2$direct_citers, 1L)
  expect_equal(m2$depth2_descendants, 1L)
  expect_equal(m2$total_depth2_reach, 2L)
  expect_equal(m2$post_notice_direct_citers, 1L)
  expect_equal(m2$post_notice_depth2_descendants, 1L)

  # Headline regression: post-notice must be NON-ZERO (Python bug returned 0).
  expect_true(sum(m$post_notice_direct_citers) > 0L)
  expect_true(sum(m$post_notice_depth2_descendants) > 0L)

  # Sorted by total_depth2_reach descending
  expect_true(all(diff(m$total_depth2_reach) <= 0L))
})

test_that("seed_metrics collapses duplicate seed records to one source paper", {
  store <- setup_test_store()
  on.exit(store$close(), add = TRUE)
  store$upsert_seed(list(
    record_id = "R1B", title = "Seed One Later Notice",
    notice_type = "Expression of concern",
    notice_date = as.Date("2019-12-01"),
    original_paper_date = as.Date("2019-01-01"),
    original_doi = "10.1000/seed1", original_pmid = NA,
    author = "A. Author", journal = "Nature", publisher = "Springer",
    subject = "Biology", reason = "concern", article_type = "Article",
    country = "US", openalex_id = S1,
    resolved_by = "test", resolved_status = "resolved", source_row_json = NA
  ))

  f <- load_test_frame(store)
  m <- seed_metrics(f$seeds, f$nodes, f$edges)

  m1 <- m[m$openalex_id == S1, ]
  expect_equal(nrow(m1), 1L)
  expect_equal(m1$seed_record_count, 2L)
  expect_match(m1$seed_record_ids, "R1")
  expect_match(m1$seed_record_ids, "R1B")
  expect_equal(m1$notice_date, as.Date("2019-12-01"))
  expect_equal(m1$direct_citers, 2L)
})

test_that("bridge_metrics returns correct depth-1 bridge counts", {
  store <- setup_test_store()
  on.exit(store$close(), add = TRUE)
  f <- load_test_frame(store)
  b <- bridge_metrics(f$nodes, f$edges)

  expect_s3_class(b, "data.frame")
  expect_true(all(c("openalex_id", "cited_seed_count",
                    "depth2_citer_count") %in% names(b)))

  # D1A cites S1 (seed) -> cited_seed_count 1; D2A cites D1A -> depth2_citer_count 1
  b1a <- b[b$openalex_id == D1A, ]
  expect_equal(b1a$cited_seed_count, 1L)
  expect_equal(b1a$depth2_citer_count, 1L)

  # D1B cites S1 -> cited_seed_count 1; no depth2 citer -> 0
  b1b <- b[b$openalex_id == D1B, ]
  expect_equal(b1b$cited_seed_count, 1L)
  expect_equal(b1b$depth2_citer_count, 0L)

  # D1C cites S2 -> cited_seed_count 1; D2B cites D1C -> depth2_citer_count 1
  b1c <- b[b$openalex_id == D1C, ]
  expect_equal(b1c$cited_seed_count, 1L)
  expect_equal(b1c$depth2_citer_count, 1L)
})

test_that("summary_metrics returns all required fields", {
  store <- setup_test_store()
  on.exit(store$close(), add = TRUE)
  f <- load_test_frame(store)
  g <- build_graph(f$nodes, f$edges)
  ts <- seed_metrics(f$seeds, f$nodes, f$edges)
  s <- summary_metrics(g, f$seeds, f$nodes, f$edges, ts)

  expected <- c("seed_records", "resolved_seed_records", "depth2_nodes",
                "depth2_edges", "depth0_nodes", "depth1_nodes",
                "depth2_nodes_only", "weak_components",
                "post_notice_direct_citers", "opencitations_edges",
                "openalex_edges", "linked_seed_records",
                "resolved_seed_papers",
                "duplicate_resolved_seed_records",
                "duplicate_seed_rows_in_duplicate_groups")
  expect_true(all(expected %in% names(s)))

  expect_equal(s$seed_records, 2L)
  expect_equal(s$resolved_seed_records, 2L)
  expect_equal(s$linked_seed_records, 2L)
  expect_equal(s$resolved_seed_papers, 2L)
  expect_equal(s$duplicate_resolved_seed_records, 0L)
  expect_equal(s$depth2_nodes, 7L)
  expect_equal(s$depth2_edges, 5L)
  expect_equal(s$depth0_nodes, 2L)
  expect_equal(s$depth1_nodes, 3L)
  expect_equal(s$depth2_nodes_only, 2L)
  expect_equal(s$openalex_edges, 5L)
  expect_equal(s$opencitations_edges, 0L)
  # Headline regression: post-notice must be > 0.
  expect_true(s$post_notice_direct_citers > 0L)
})

test_that("run_analysis end-to-end writes all output files", {
  store <- setup_test_store()
  on.exit(store$close(), add = TRUE)
  out <- withr::local_tempdir()
  summary <- run_analysis(store, out, max_analysis_depth = 2L)

  tdir <- file.path(out, "tables")
  fdir <- file.path(out, "figures")
  gdir <- file.path(out, "graphs")

  expected_tables <- c("summary.csv", "depth_counts.csv", "top_polluted_seeds.csv",
                       "bridge_papers.csv", "topic_distribution.csv",
                       "nodes_depth2.csv", "edges_depth2.csv")
  for (fn in expected_tables) {
    expect_true(file.exists(file.path(tdir, fn)),
                info = paste("missing table:", fn))
  }
  expect_true(file.exists(file.path(gdir, "network_depth2.graphml")))

  expected_fig <- c("depth_counts.png", "top_polluted_seeds.png",
                    "post_notice_timeline.png",
                    "depth_counts.svg", "top_polluted_seeds.svg",
                    "post_notice_timeline.svg")
  for (fn in expected_fig) {
    expect_true(file.exists(file.path(fdir, fn)),
                info = paste("missing figure:", fn))
  }

  # Headline regression: summary post-notice > 0.
  expect_true(summary$post_notice_direct_citers > 0L)

  # Summary CSV on disk mirrors the returned list.
  summary_df <- readr::read_csv(file.path(tdir, "summary.csv"),
                                show_col_types = FALSE)
  on_disk <- setNames(as.integer(summary_df$value), summary_df$metric)
  expect_equal(on_disk[["post_notice_direct_citers"]],
               as.integer(summary$post_notice_direct_citers))
})

test_that("streaming GraphML export writes a complete compressed graph", {
  store <- setup_test_store()
  on.exit(store$close(), add = TRUE)
  graph_dir <- withr::local_tempdir()

  exported <- export_full_network_graph_graphml_gz(
    con = store$con,
    graph_dir = graph_dir,
    max_analysis_depth = 2L,
    scale_counts = list(nodes = 7L, exported_edges = 5L),
    chunk_size = 2L
  )

  expect_true(file.exists(exported$graphml_gz))
  expect_true(file.exists(exported$manifest))
  expect_equal(exported$node_count, 7L)
  expect_equal(exported$edge_count, 5L)

  graph_con <- gzfile(exported$graphml_gz, open = "rt")
  withr::defer(close(graph_con))
  graph_lines <- readLines(graph_con, warn = FALSE)
  expect_true(any(grepl("<graphml", graph_lines, fixed = TRUE)))
  expect_true(any(grepl(paste0("<node id=\"", S1, "\""), graph_lines,
                        fixed = TRUE)))
  expect_true(any(grepl(paste0("source=\"", D1A, "\" target=\"", S1, "\""),
                        graph_lines, fixed = TRUE)))
  expect_true(any(grepl("<data key=\"n_depth\">0</data>", graph_lines,
                        fixed = TRUE)))

  manifest <- jsonlite::read_json(exported$manifest)
  expect_equal(manifest$format, "GraphML")
  expect_equal(manifest$compression, "gzip")
  expect_equal(manifest$node_count, 7L)
  expect_equal(manifest$edge_count, 5L)
})
