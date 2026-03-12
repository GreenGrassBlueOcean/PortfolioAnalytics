# Regression tests for extractStats.optimize.portfolio.DEoptim fallback path.
# When DEoptim runs in parallel, the storage environment is serialized to
# worker processes and the main process never accumulates per-iteration
# results, leaving DEoptim_objective_results as an empty list.

library(testthat)
library(PortfolioAnalytics)

# --- Helper: build a minimal mock DEoptim result object ---
make_mock_deoptim_result <- function(weights, objective_measures, out,
                                     deoptim_results = list()) {
  obj <- list(
    weights = weights,
    objective_measures = objective_measures,
    out = out,
    DEoptim_objective_results = deoptim_results
  )
  class(obj) <- "optimize.portfolio.DEoptim"
  obj
}

test_that("extractStats fallback returns a single-row matrix when DEoptim_objective_results is empty", {
  w <- c(A = 0.4, B = 0.35, C = 0.25)
  obj_measures <- list(StdDev = 0.015, mean = 0.006)
  mock <- make_mock_deoptim_result(w, obj_measures, out = 0.015)

  result <- extractStats(mock)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 1)
  expect_equal(rownames(result), "DE.portf.optimal")
})

test_that("fallback matrix contains correct objective measures, out, and weights", {
  w <- c(A = 0.4, B = 0.35, C = 0.25)
  obj_measures <- list(StdDev = 0.015, mean = 0.006)
  mock <- make_mock_deoptim_result(w, obj_measures, out = 0.015)

  result <- extractStats(mock)

  expect_equal(result[1, "StdDev"], 0.015)
  expect_equal(result[1, "mean"], 0.006)
  expect_equal(result[1, "out"], 0.015)
  expect_equal(result[1, "w.A"], 0.4)
  expect_equal(result[1, "w.B"], 0.35)
  expect_equal(result[1, "w.C"], 0.25)
})

test_that("fallback respects the prefix argument", {
  w <- c(X = 0.5, Y = 0.5)
  mock <- make_mock_deoptim_result(w, list(ES = 0.03), out = 0.03)

  result <- extractStats(mock, prefix = "test_")

  expect_equal(rownames(result), "test_DE.portf.optimal")
})

test_that("fallback column count equals objectives + 1 (out) + n_assets", {
  w <- c(A = 0.25, B = 0.25, C = 0.25, D = 0.25)
  obj_measures <- list(StdDev = 0.02, mean = 0.005, ES = 0.04)
  mock <- make_mock_deoptim_result(w, obj_measures, out = 0.02)

  result <- extractStats(mock)

  # 3 objectives + 1 out + 4 weights = 8

  expect_equal(ncol(result), 8)
  expect_equal(
    colnames(result),
    c("StdDev", "mean", "ES", "out", "w.A", "w.B", "w.C", "w.D")
  )
})

test_that("extractStats still errors when DEoptim_objective_results is NULL (trace=FALSE)", {
  w <- c(A = 0.5, B = 0.5)
  mock <- make_mock_deoptim_result(w, list(StdDev = 0.01), out = 0.01,
                                    deoptim_results = NULL)
  mock$DEoptim_objective_results <- NULL

  expect_error(extractStats(mock), "trace=TRUE must be specified")
})

test_that("extractStats uses populated results when available (non-fallback path)", {
  w <- c(A = 0.6, B = 0.4)
  obj_measures <- list(StdDev = 0.02, mean = 0.008)
  # Two populated iteration results
  iter_results <- list(
    list(objective_measures = list(StdDev = 0.025, mean = 0.007),
         out = 0.025, weights = c(0.5, 0.5)),
    list(objective_measures = list(StdDev = 0.020, mean = 0.008),
         out = 0.020, weights = c(0.6, 0.4))
  )
  mock <- make_mock_deoptim_result(w, obj_measures, out = 0.02,
                                    deoptim_results = iter_results)

  result <- extractStats(mock)

  expect_equal(nrow(result), 2)
  expect_equal(result[2, "StdDev"], 0.020)
  expect_equal(result[2, "w.A"], 0.6)
})
