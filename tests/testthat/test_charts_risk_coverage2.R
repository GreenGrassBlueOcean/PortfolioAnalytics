##### test_charts_risk_coverage2.R #####
# Phase 3 coverage: chart.RiskBudget remaining paths
# Targets: min_prisk point, rebalancing percentage, xlab styling


data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# Risk budget portfolio with min_prisk and max_prisk targets
portf_rb <- portfolio.spec(assets = colnames(R5))
portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                          min_prisk = rep(0.05, 5),
                          max_prisk = rep(0.40, 5),
                          min_concentration = TRUE)

set.seed(2917)
opt_rb <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                             search_size = 500, trace = TRUE)

# ===========================================================================
# A. chart.RiskBudget — min_prisk / max_prisk points
# ===========================================================================

test_that("chart.RiskBudget percentage with min_prisk and max_prisk plots bounds", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskBudget(opt_rb, risk.type = "percentage"))
})

# ===========================================================================
# B. chart.RiskBudget — styling edge cases
# ===========================================================================

test_that("chart.RiskBudget with xlab triggers non-NULL margin path", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, risk.type = "absolute", xlab = "Assets")
  )
})

test_that("chart.RiskBudget with main='' triggers empty title margin", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, risk.type = "absolute", main = "")
  )
})

test_that("chart.RiskBudget with las=0 triggers low-las margin path", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, risk.type = "absolute", las = 0)
  )
})

# ===========================================================================
# C. chart.RiskBudget.rebalancing — percentage risk type
# ===========================================================================

test_that("chart.RiskBudget rebalancing percentage risk type", {
  set.seed(6214)
  opt_rebal <- optimize.portfolio.rebalancing(
    R5, portf_rb, optimize_method = "random",
    search_size = 200, rebalance_on = "quarters",
    training_period = 24, trace = TRUE
  )
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rebal, match.col = "StdDev", risk.type = "percentage")
  )
})
