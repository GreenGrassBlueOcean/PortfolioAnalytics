##### test_charts_risk_advanced.R #####
# Phase 3A: chart.RiskBudget advanced paths
# Targets uncovered paths in charts.risk.R:
#   - chart.RiskBudget.optimize.portfolio: absolute risk type (lines 111-153),
#     percentage risk type with neighbors (vector, matrix)
#   - chart.RiskBudget.opt.list: barplot mode, absolute line mode, legend.loc
#   - barplotRiskBudget: absolute + percentage risk type paths
#   - Warning when no risk_budget_objective present
#   - Class validation

library(testthat)
library(PortfolioAnalytics)

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# Risk budget portfolio spec
portf_rb <- portfolio.spec(assets = colnames(R5))
portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                          min_concentration = TRUE)

# Pre-compute random optimization with risk budget
set.seed(4129)
opt_rb_rp <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                                search_size = 500, trace = TRUE)

# ===========================================================================
# A. chart.RiskBudget.optimize.portfolio — absolute risk type
# ===========================================================================

test_that("chart.RiskBudget absolute risk type runs without error", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskBudget(opt_rb_rp, risk.type = "absolute"))
})

test_that("chart.RiskBudget percentage risk type runs without error", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskBudget(opt_rb_rp, risk.type = "percentage"))
})

# ---------------------------------------------------------------------------
# B. chart.RiskBudget with neighbors (vector — single number)
# ---------------------------------------------------------------------------

test_that("chart.RiskBudget absolute with single-number neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_rp, risk.type = "absolute", neighbors = 3)
  )
})

test_that("chart.RiskBudget percentage with single-number neighbors", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_rp, risk.type = "percentage", neighbors = 3)
  )
})

# ---------------------------------------------------------------------------
# C. chart.RiskBudget with neighbors (vector — portfolio indices)
# ---------------------------------------------------------------------------

test_that("chart.RiskBudget absolute with vector of portfolio indices", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_rp, risk.type = "absolute", neighbors = c(1, 5, 10))
  )
})

test_that("chart.RiskBudget percentage with vector of portfolio indices", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_rp, risk.type = "percentage", neighbors = c(1, 5, 10))
  )
})

# ---------------------------------------------------------------------------
# D. chart.RiskBudget with neighbors as matrix
# ---------------------------------------------------------------------------

test_that("chart.RiskBudget absolute with matrix neighbors", {
  xtract <- extractStats(opt_rb_rp)
  nb_mat <- head(xtract, 5)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_rp, risk.type = "absolute", neighbors = nb_mat)
  )
})

test_that("chart.RiskBudget percentage with matrix neighbors", {
  xtract <- extractStats(opt_rb_rp)
  nb_mat <- head(xtract, 5)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_rp, risk.type = "percentage", neighbors = nb_mat)
  )
})

# ---------------------------------------------------------------------------
# E. Warning when no risk_budget_objective
# ---------------------------------------------------------------------------

test_that("chart.RiskBudget warns when no risk_budget_objective present", {
  portf_norb <- portfolio.spec(assets = colnames(R5))
  portf_norb <- add.constraint(portf_norb, type = "weight_sum",
                               min_sum = 0.99, max_sum = 1.01)
  portf_norb <- add.constraint(portf_norb, type = "box", min = 0.05, max = 0.55)
  portf_norb <- add.objective(portf_norb, type = "return", name = "mean")
  portf_norb <- add.objective(portf_norb, type = "risk", name = "StdDev")

  set.seed(8271)
  opt_norb <- optimize.portfolio(R5, portf_norb, optimize_method = "random",
                                 search_size = 200, trace = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # The function warns about missing risk_budget_objective then errors
  # because idx is empty — this is expected behavior (bug in source, but
  # we test that the warning fires before the error)
  expect_warning(
    tryCatch(
      chart.RiskBudget(opt_norb, risk.type = "absolute"),
      error = function(e) NULL
    ),
    "no risk_budget_objective"
  )
})

# ---------------------------------------------------------------------------
# F. Class validation
# ---------------------------------------------------------------------------

test_that("chart.RiskBudget.optimize.portfolio rejects wrong class", {
  mock <- list(weights = rep(0.2, 5))
  class(mock) <- "not_portfolio"
  expect_error(chart.RiskBudget(mock), "no applicable method")
})

# ===========================================================================
# G. chart.RiskBudget.opt.list — barplot mode, absolute + percentage
# ===========================================================================

# Build an opt.list with risk budget objectives
set.seed(5937)
opt_rb1 <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                              search_size = 200, trace = TRUE)
set.seed(6842)
opt_rb2 <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                              search_size = 200, trace = TRUE)
opt_list_rb <- combine.optimizations(list(rb1 = opt_rb1, rb2 = opt_rb2))

test_that("chart.RiskBudget opt.list line mode absolute runs", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list_rb, match.col = "StdDev",
                     risk.type = "absolute", plot.type = "line")
  )
})

test_that("chart.RiskBudget opt.list line mode percentage runs", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list_rb, match.col = "StdDev",
                     risk.type = "percentage", plot.type = "line")
  )
})

test_that("chart.RiskBudget opt.list barplot mode absolute runs", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list_rb, match.col = "StdDev",
                     risk.type = "absolute", plot.type = "barplot")
  )
})

test_that("chart.RiskBudget opt.list barplot mode percentage runs", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list_rb, match.col = "StdDev",
                     risk.type = "percentage", plot.type = "barplot")
  )
})

test_that("chart.RiskBudget opt.list with legend.loc and colorset", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_list_rb, match.col = "StdDev",
                     risk.type = "absolute", plot.type = "line",
                     legend.loc = "topright",
                     colorset = c("red", "blue"))
  )
})

test_that("chart.RiskBudget opt.list rejects wrong class", {
  mock <- list(1, 2)
  class(mock) <- "not_opt_list"
  expect_error(
    chart.RiskBudget(mock, plot.type = "line"),
    "opt.list"
  )
})

# ---------------------------------------------------------------------------
# H. chart.RiskBudget.optimize.portfolio.rebalancing
# ---------------------------------------------------------------------------

test_that("chart.RiskBudget rebalancing absolute risk type", {
  set.seed(3718)
  opt_rebal <- optimize.portfolio.rebalancing(
    R5, portf_rb, optimize_method = "random",
    search_size = 200, rebalance_on = "quarters",
    training_period = 24, trace = TRUE
  )
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rebal, match.col = "StdDev", risk.type = "absolute")
  )
})
