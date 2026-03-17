context("charts.risk.R coverage: chart.RiskBudget additional paths")

library(testthat)
library(PortfolioAnalytics)

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# Portfolio with risk budget objective
portf_rb <- portfolio.spec(assets = colnames(R5))
portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                          max_prisk = 0.5)

set.seed(2847)
opt_rb <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                             search_size = 500, trace = TRUE)

# Plain portfolio without risk_budget_objective (for warning path)
portf_plain <- portfolio.spec(assets = colnames(R5))
portf_plain <- add.constraint(portf_plain, type = "weight_sum",
                              min_sum = 0.99, max_sum = 1.01)
portf_plain <- add.constraint(portf_plain, type = "box", min = 0.05, max = 0.55)
portf_plain <- add.objective(portf_plain, type = "risk", name = "StdDev")
portf_plain <- add.objective(portf_plain, type = "return", name = "mean")

set.seed(6193)
opt_plain <- optimize.portfolio(R5, portf_plain, optimize_method = "random",
                                search_size = 500, trace = TRUE)

# ============================================================================
# A. chart.RiskBudget with risk.type = "percentage"
# ============================================================================

test_that("chart.RiskBudget with percentage risk type", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev", risk.type = "percentage")
  )
})

# ============================================================================
# B. chart.RiskBudget with risk.type = "absolute"
# ============================================================================

test_that("chart.RiskBudget with absolute risk type", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev", risk.type = "absolute")
  )
})

# ============================================================================
# C. chart.RiskBudget barplot mode
# ============================================================================

test_that("chart.RiskBudget barplot mode", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev", risk.type = "percentage",
                     plot.type = "barplot")
  )
})

# ============================================================================
# D. chart.RiskBudget with neighbors
# ============================================================================

test_that("chart.RiskBudget with neighbors as integer", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev", risk.type = "percentage",
                     neighbors = 3)
  )
})

# ============================================================================
# E. Warning when no risk_budget_objective
# ============================================================================

test_that("chart.RiskBudget warns without risk_budget_objective", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_warning(
    tryCatch(
      chart.RiskBudget(opt_plain, match.col = "StdDev", risk.type = "percentage"),
      error = function(e) NULL
    ),
    "risk_budget"
  )
})

# ============================================================================
# F. chart.RiskBudget with custom styling
# ============================================================================

test_that("chart.RiskBudget with custom colors and legend", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb, match.col = "StdDev", risk.type = "percentage",
                     main = "Risk Budget", las = 1, cex.axis = 1.0,
                     element.color = "black")
  )
})
