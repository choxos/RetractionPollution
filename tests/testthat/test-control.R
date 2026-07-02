library(testthat)

for (mod in c("util.R", "storage.R", "manuscript.R", "control.R")) {
  p <- file.path(testthat::test_path("..", "..", "R", mod))
  if (file.exists(p)) source(p)
}

test_that("match_controls_nn matches within year on nearest log in-degree", {
  pool <- data.frame(
    openalex_id = c("c1", "c2", "c3", "c4", "c5"),
    year = 2020L,
    indeg = c(1L, 10L, 100L, 1000L, 10000L)
  )
  pool$logdeg <- log1p(pool$indeg)
  seeds <- data.frame(
    openalex_id = c("s_hi", "s_lo"),
    year = 2020L,
    indeg = c(90L, 5L)
  )
  seeds$logdeg <- log1p(seeds$indeg)

  m <- match_controls_nn(seeds, pool)
  expect_equal(nrow(m), 2L)
  # indeg 90 (log 4.51) is nearest to indeg 100 (log 4.615).
  expect_equal(m$control_id[m$openalex_id == "s_hi"], "c3")
  # indeg 5 (log 1.79) is nearest to indeg 10 (log 2.40), not indeg 1 (log 0.69).
  expect_equal(m$control_id[m$openalex_id == "s_lo"], "c2")
})

test_that("match_controls_nn stratifies by exact year and skips empty strata", {
  pool <- data.frame(openalex_id = c("a", "b"), year = c(2019L, 2019L),
                     indeg = c(5L, 50L))
  pool$logdeg <- log1p(pool$indeg)
  seeds <- data.frame(openalex_id = c("s1", "s2"), year = c(2019L, 2021L),
                      indeg = c(6L, 6L))
  seeds$logdeg <- log1p(seeds$indeg)

  m <- match_controls_nn(seeds, pool)
  # 2021 seed has no same-year control and is dropped.
  expect_equal(nrow(m), 1L)
  expect_equal(m$openalex_id, "s1")
  expect_equal(m$control_id, "a")
})
