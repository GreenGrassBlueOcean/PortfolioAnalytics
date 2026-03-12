##### test_solver_registry.R #####
# Tests for Proposal #6: Solver dispatch registry

library(testthat)
library(PortfolioAnalytics)

context("Solver registry and dispatch")

# ---- Registry lookup ----

test_that("get_solver finds all built-in solvers", {
  expect_true(is.function(get_solver("DEoptim")))
  expect_true(is.function(get_solver("random")))
  expect_true(is.function(get_solver("ROI")))
  expect_true(is.function(get_solver("pso")))
  expect_true(is.function(get_solver("GenSA")))
})

test_that("ROI sub-solver aliases resolve to the same function", {
  roi_fn <- get_solver("ROI")
  expect_identical(get_solver("quadprog"), roi_fn)
  expect_identical(get_solver("glpk"), roi_fn)
  expect_identical(get_solver("symphony"), roi_fn)
  expect_identical(get_solver("ipop"), roi_fn)
})

test_that("get_solver returns NULL for unknown methods", {
  expect_null(get_solver("nonexistent_solver"))
})

# ---- Custom solver registration ----

test_that("register_solver adds a custom solver", {
  dummy_solver <- function(R, portfolio, constraints, moments, penalty,
                           N, call, trace, search_size, rp, message = FALSE, ...) {
    list(weights = rep(1 / N, N), objective_measures = list(),
         opt_values = list(), out = 0, call = call)
  }
  register_solver("test_dummy", dummy_solver)
  expect_true(is.function(get_solver("test_dummy")))
  expect_identical(get_solver("test_dummy"), dummy_solver)
})

test_that("register_solver validates arguments", {
  expect_error(register_solver(123, identity), "'name' must be a single character string")
  expect_error(register_solver("foo", "not_a_function"), "'fn' must be a function")
})

# ---- normalize_portfolio_weights ----

test_that("normalize_portfolio_weights scales to max_sum", {
  w <- c(0.4, 0.4, 0.4)
  cons <- list(min_sum = 0.99, max_sum = 1.01)
  nw <- normalize_portfolio_weights(w, cons)
  expect_true(sum(nw) <= 1.01 + .Machine$double.eps)
})

test_that("normalize_portfolio_weights scales to min_sum", {
  w <- c(0.1, 0.1, 0.1)
  cons <- list(min_sum = 0.99, max_sum = 1.01)
  nw <- normalize_portfolio_weights(w, cons)
  expect_true(sum(nw) >= 0.99 - .Machine$double.eps)
})

test_that("normalize_portfolio_weights is no-op when within bounds", {
  w <- c(0.3, 0.3, 0.4)
  cons <- list(min_sum = 0.99, max_sum = 1.01)
  nw <- normalize_portfolio_weights(w, cons)
  expect_equal(nw, w)
})

# ---- Integration: dispatch produces correct classes ----

test_that("dispatch produces correct class for each solver", {
  data(edhec)
  R <- edhec[1:60, 1:4]
  funds <- colnames(R)
  portf <- portfolio.spec(assets = funds)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  set.seed(4172)
  opt_rp <- optimize.portfolio(R, portf, optimize_method = "random",
                               search_size = 50, trace = FALSE)
  expect_s3_class(opt_rp, "optimize.portfolio.random")
  expect_s3_class(opt_rp, "optimize.portfolio")

  opt_roi <- optimize.portfolio(R, portf, optimize_method = "ROI")
  expect_s3_class(opt_roi, "optimize.portfolio.ROI")
  expect_s3_class(opt_roi, "optimize.portfolio")
})
