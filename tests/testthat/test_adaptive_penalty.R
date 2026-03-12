##### test_adaptive_penalty.R #####
# Tests for Proposal #5: Adaptive penalty scaling

library(testthat)
library(PortfolioAnalytics)

context("Adaptive penalty scaling")

# ---- Setup ----

data(edhec)
R <- edhec[1:60, 1:4]
funds <- colnames(R)

# Basic portfolio: min variance with full investment + box constraints
base_portf <- portfolio.spec(assets = funds)
base_portf <- add.constraint(base_portf, type = "full_investment")
base_portf <- add.constraint(base_portf, type = "box",
                             min = 0.05, max = 0.65)
base_portf <- add.objective(base_portf, type = "risk", name = "StdDev")


# ---- constrained_objective penalty formal argument ----

test_that("constrained_objective accepts penalty as a formal argument", {
  w <- rep(1 / length(funds), length(funds))
  names(w) <- funds

  # Default penalty = 1e4
  val_default <- constrained_objective(w = w, R = R, portfolio = base_portf)
  expect_true(is.finite(val_default))

  # Explicit penalty = 500
  val_500 <- constrained_objective(w = w, R = R, portfolio = base_portf,
                                   penalty = 500)
  expect_true(is.finite(val_500))

  # penalty = 0 zeros out all constraint penalty terms

  # For a feasible portfolio, constraint penalties are zero regardless,
  # so the values should be the same
  val_0 <- constrained_objective(w = w, R = R, portfolio = base_portf,
                                 penalty = 0)
  expect_true(is.finite(val_0))
})

test_that("penalty=0 zeroes out constraint penalty contributions", {
  # Create weights that violate box constraints
  w_bad <- c(0.90, 0.05, 0.03, 0.02)
  names(w_bad) <- funds

  val_high <- constrained_objective(w = w_bad, R = R, portfolio = base_portf,
                                    penalty = 1e4, normalize = FALSE)
  val_zero <- constrained_objective(w = w_bad, R = R, portfolio = base_portf,
                                    penalty = 0, normalize = FALSE)

  # With penalty=0, constraint violations contribute nothing, so the value

  # should be smaller (just the objective, no penalty terms)
  expect_true(val_high > val_zero)
})

test_that("constrained_objective_v1 also accepts penalty formal", {
  expect_true("penalty" %in% names(formals(constrained_objective_v1)))
  expect_equal(formals(constrained_objective_v1)$penalty, 1e4)
})


# ---- calibrate_penalty ----

test_that("calibrate_penalty returns a positive finite number", {
  p <- calibrate_penalty(R = R, portfolio = base_portf)
  expect_true(is.numeric(p))
  expect_length(p, 1)
  expect_true(is.finite(p))
  expect_true(p > 0)
})

test_that("calibrate_penalty respects scaling_factor argument", {
  p1 <- calibrate_penalty(R = R, portfolio = base_portf, scaling_factor = 100)
  p2 <- calibrate_penalty(R = R, portfolio = base_portf, scaling_factor = 100000)

  # With a much larger scaling factor, the penalty should be larger
  # (unless min_penalty dominates both, which is unlikely for real data)
  expect_true(p2 >= p1)
})

test_that("calibrate_penalty respects min_penalty argument", {
  p <- calibrate_penalty(R = R, portfolio = base_portf, min_penalty = 999)
  expect_true(p >= 999)
})

test_that("calibrate_penalty falls back to 1e4 on degenerate input", {
  # Portfolio with no objectives should produce obj_scale=0
  empty_portf <- portfolio.spec(assets = funds)
  empty_portf <- add.constraint(empty_portf, type = "full_investment")
  empty_portf <- add.constraint(empty_portf, type = "box",
                                min = 0.05, max = 0.65)
  # No objectives added

  p <- calibrate_penalty(R = R, portfolio = empty_portf)
  expect_equal(p, 1e4)
})

test_that("calibrate_penalty scales with objective magnitude", {
  # Portfolio minimizing StdDev (small scale, ~0.01-0.05)
  risk_portf <- portfolio.spec(assets = funds)
  risk_portf <- add.constraint(risk_portf, type = "full_investment")
  risk_portf <- add.constraint(risk_portf, type = "box",
                                min = 0.05, max = 0.65)
  risk_portf <- add.objective(risk_portf, type = "risk", name = "StdDev")

  p_risk <- calibrate_penalty(R = R, portfolio = risk_portf)

  # Portfolio maximizing return with high multiplier (larger scale)
  ret_portf <- portfolio.spec(assets = funds)
  ret_portf <- add.constraint(ret_portf, type = "full_investment")
  ret_portf <- add.constraint(ret_portf, type = "box",
                               min = 0.05, max = 0.65)
  ret_portf <- add.objective(ret_portf, type = "return", name = "mean",
                              multiplier = -100)

  p_ret <- calibrate_penalty(R = R, portfolio = ret_portf)

  # With a 100x multiplier on return, the penalty should be notably larger
  expect_true(p_ret > p_risk)
})


# ---- Integration with optimize.portfolio ----

test_that("optimize.portfolio accepts penalty='auto' (default)", {
  set.seed(4827)
  opt <- optimize.portfolio(R, base_portf,
                            optimize_method = "random",
                            search_size = 50,
                            trace = FALSE)
  expect_s3_class(opt, "optimize.portfolio")
  expect_true(!is.null(opt$penalty))
  expect_true(is.numeric(opt$penalty))
  expect_true(opt$penalty > 0)
})

test_that("optimize.portfolio accepts explicit numeric penalty", {
  set.seed(6143)
  opt <- optimize.portfolio(R, base_portf,
                            optimize_method = "random",
                            search_size = 50,
                            trace = FALSE,
                            penalty = 500)
  expect_equal(opt$penalty, 500)
})

test_that("calibrated penalty differs from 1e4 for small-scale objectives", {
  set.seed(9251)
  opt <- optimize.portfolio(R, base_portf,
                            optimize_method = "random",
                            search_size = 50,
                            trace = FALSE)
  # For StdDev objective on monthly returns (~0.01-0.05 scale),
  # the calibrated penalty should be much less than 1e4
  expect_true(opt$penalty < 1e4)
})
