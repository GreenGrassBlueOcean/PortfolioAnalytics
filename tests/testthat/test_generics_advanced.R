library(testthat)
library(PortfolioAnalytics)

context("generics advanced: remaining uncovered paths in generics.R and constraints.R")

# ============================================================================
# Shared test data
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# ============================================================================
# A. update.constraint method
# ============================================================================

test_that("update.constraint modifies existing constraint parameters", {
  spec <- portfolio.spec(assets = colnames(R5))
  spec <- add.constraint(spec, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  constr <- spec$constraints[[1]]

  # update.constraint should work without error
  updated <- update(constr, min_sum = 0.95)
  expect_true(is.constraint(updated))
})

test_that("update.constraint errors on non-constraint input", {
  expect_error(update.constraint(list()), "class constraints")
})

test_that("update.constraint errors when no call component", {
  c1 <- structure(list(type = "test", enabled = TRUE), class = "constraint")
  expect_error(update(c1), "need an object with call component")
})

# ============================================================================
# B. update_constraint_v1tov2
# ============================================================================

test_that("update_constraint_v1tov2 converts v1 to v2 portfolio", {
  spec <- portfolio.spec(assets = 4)
  suppressWarnings({
    v1 <- constraint_v1(assets = 4, min = 0.1, max = 0.5,
                        min_sum = 0.99, max_sum = 1.01)
  })
  result <- update_constraint_v1tov2(spec, v1)
  expect_true(is.portfolio(result))
  constr <- get_constraints(result)
  expect_equal(constr$min_sum, 0.99)
  expect_equal(constr$max_sum, 1.01)
  expect_equal(as.numeric(constr$min), rep(0.1, 4))
  expect_equal(as.numeric(constr$max), rep(0.5, 4))
})

test_that("update_constraint_v1tov2 errors on wrong types", {
  spec <- portfolio.spec(assets = 4)
  expect_error(
    update_constraint_v1tov2(list(), list()),
    "class 'portfolio'"
  )
  expect_error(
    update_constraint_v1tov2(spec, list()),
    "class 'v1_constraint'"
  )
})

# ============================================================================
# C. >10 category labels truncation in print.portfolio
# ============================================================================

test_that("print.portfolio truncates >10 category labels", {
  R13 <- edhec[1:48, ]
  colnames(R13) <- paste0("Asset", seq_len(ncol(R13)))

  # category_labels must be a character vector of same length as assets
  n_assets <- ncol(R13)
  cat_vec <- paste0("Cat", rep(1:n_assets, length.out = n_assets))

  spec <- portfolio.spec(assets = colnames(R13), category_labels = cat_vec)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "long_only")

  out <- capture.output(print(spec))
  expect_true(any(grepl("Category Labels", out)))
  if (n_assets > 10) {
    expect_true(any(grepl("More than 10 categories", out)))
  }
})

# ============================================================================
# D. print.optimize.portfolio.ROI with HHI concentration nested measures
# ============================================================================

test_that("print.optimize.portfolio.ROI shows nested HHI measures when present", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  # ROI folds HHI into QP penalty, so build a fake ROI result with nested measures
  # to exercise the inner loop in print.optimize.portfolio.ROI
  portf_basic <- portfolio.spec(assets = colnames(R5))
  portf_basic <- add.constraint(portf_basic, type = "full_investment")
  portf_basic <- add.constraint(portf_basic, type = "box", min = 0.05, max = 0.55)
  portf_basic <- add.objective(portf_basic, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R5, portf_basic, optimize_method = "ROI", trace = TRUE)

  # Inject nested objective_measures to exercise the inner print loop
  opt$objective_measures <- list(
    StdDev = list(
      StdDev = 0.015,
      contribution = setNames(rep(0.003, 5), colnames(R5))
    )
  )
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("Objective Measure", out)))
  expect_true(any(grepl("contribution", out)))
})

# ============================================================================
# E. print.summary.optimize.portfolio with NULL objective_values fallback
# ============================================================================

test_that("print.summary handles NULL objective_values via out field", {
  # Create a fake summary object with NULL objective_values but with $out
  fake_summary <- structure(list(
    call = quote(optimize.portfolio()),
    weights = c(A = 0.3, B = 0.3, C = 0.2, D = 0.1, E = 0.1),
    objective_values = NULL,
    out = 0.05,
    elapsed_time = 1.5,
    initial_weights = rep(0.2, 5),
    portfolio = {
      p <- portfolio.spec(assets = colnames(R5))
      p <- add.constraint(p, type = "full_investment")
      p <- add.constraint(p, type = "long_only")
      p <- add.objective(p, type = "risk", name = "StdDev")
      p
    },
    leverage_constraint = list(min_sum = 1, max_sum = 1, actual = 1),
    box_constraint = list(min = rep(0, 5), max = rep(1, 5)),
    position_limit_constraint = list(
      max_pos = NULL, max_pos_long = NULL, max_pos_short = NULL,
      max_pos_actual = 5, max_pos_long_actual = 5, max_pos_short_actual = 0
    ),
    diversification_constraint = list(
      diversification_target = NULL,
      diversification_actual = 0.78
    ),
    turnover_constraint = list(turnover_target = NULL, turnover_actual = 0.3)
  ), class = "summary.optimize.portfolio")

  out <- capture.output(print(fake_summary))
  expect_true(any(grepl("Objective Measures", out)))
  expect_true(any(grepl("Unconstrained", out)))
})

# ============================================================================
# F. summary.portfolio with no constraints or objectives
# ============================================================================

test_that("summary.portfolio handles portfolio with no constraints", {
  spec <- portfolio.spec(assets = colnames(R5))
  s <- summary(spec)
  expect_s3_class(s, "summary.portfolio")
  expect_length(s$enabled_constraints, 0)
  expect_length(s$disabled_constraints, 0)
  expect_length(s$enabled_objectives, 0)
  expect_length(s$disabled_objectives, 0)
})

# ============================================================================
# G. print.portfolio with no constraints/objectives sections
# ============================================================================

test_that("print.portfolio with no constraints shows no constraint section", {
  spec <- portfolio.spec(assets = colnames(R5))
  out <- capture.output(print(spec))
  expect_true(any(grepl("Number of assets: 5", out)))
  # Should not show "Enabled constraint types" since there are none
  expect_false(any(grepl("Enabled constraint types", out)))
})

test_that("print.portfolio errors on non-portfolio input", {
  expect_error(print.portfolio(list()), "not of class 'portfolio'")
})

# ============================================================================
# H. summary.optimize.portfolio.rebalancing error path
# ============================================================================

test_that("summary.optimize.portfolio.rebalancing errors on wrong class", {
  expect_error(
    summary.optimize.portfolio.rebalancing(list()),
    "not of class optimize.portfolio.rebalancing"
  )
})

# ============================================================================
# I. print.efficient.frontier error path
# ============================================================================

test_that("print.efficient.frontier errors on wrong class", {
  expect_error(
    print.efficient.frontier(list()),
    "not of class 'efficient.frontier'"
  )
})

test_that("summary.efficient.frontier errors on wrong class", {
  expect_error(
    summary.efficient.frontier(list()),
    "not of class 'efficient.frontier'"
  )
})

# ============================================================================
# J. summary.portfolio error path
# ============================================================================

test_that("summary.portfolio errors on non-portfolio input", {
  expect_error(summary.portfolio(list()), "not of class 'portfolio'")
})

# ============================================================================
# K. DEoptim print with risk budget nested measures
# ============================================================================

test_that("print.optimize.portfolio.DEoptim shows nested risk budget measures", {
  skip_if_not_installed("DEoptim")

  portf_rb <- portfolio.spec(assets = colnames(R5))
  portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                              min_sum = 0.99, max_sum = 1.01)
  portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
  portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                            min_prisk = 0.05, max_prisk = 0.40)

  set.seed(5329)
  opt <- optimize.portfolio(R5, portf_rb, optimize_method = "DEoptim",
                            search_size = 500, trace = TRUE)
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  # Risk budget should show contribution or pct_contrib
  expect_true(any(grepl("contribution", out) | grepl("pct_contrib", out)))
})

# ============================================================================
# L. GenSA print with risk budget nested measures
# ============================================================================

test_that("print.optimize.portfolio.GenSA shows nested risk budget measures", {
  skip_if_not_installed("GenSA")

  portf_rb <- portfolio.spec(assets = colnames(R5))
  portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                              min_sum = 0.99, max_sum = 1.01)
  portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
  portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                            min_prisk = 0.05, max_prisk = 0.40)

  set.seed(9718)
  opt <- optimize.portfolio(R5, portf_rb, optimize_method = "GenSA",
                            trace = TRUE)
  out <- capture.output(print(opt))
  expect_true(any(grepl("Optimal Weights", out)))
  expect_true(any(grepl("contribution", out) | grepl("pct_contrib", out)))
})
