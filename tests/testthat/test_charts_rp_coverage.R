context("charts.RP.R coverage: Weight, Scatter, plot for random portfolios")

library(testthat)
library(PortfolioAnalytics)

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "risk", name = "StdDev")
portf <- add.objective(portf, type = "return", name = "mean")

set.seed(5147)
opt_rp <- optimize.portfolio(R5, portf, optimize_method = "random",
                             search_size = 500, trace = TRUE)

# ============================================================================
# A. chart.Weight.RP — line plot
# ============================================================================

test_that("chart.Weight.RP line plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_rp))
})

# ============================================================================
# B. chart.Weight.RP — bar plot
# ============================================================================

test_that("chart.Weight.RP bar plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_rp, plot.type = "bar"))
})

# ============================================================================
# C. chart.Scatter.RP / chart.RiskReward
# ============================================================================

test_that("chart.RiskReward.RP scatter plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_rp))
})

test_that("chart.RiskReward.RP with chart.assets=TRUE", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_rp, chart.assets = TRUE))
})

# ============================================================================
# D. plot.optimize.portfolio.random (combined layout)
# ============================================================================

test_that("plot method for random portfolio optimization works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(opt_rp))
})

# ============================================================================
# E. Neighbors parameter
# ============================================================================

test_that("chart.RiskReward.RP with neighbors as integer", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_rp, neighbors = 3))
})

test_that("chart.RiskReward.RP with neighbors as vector", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_rp, neighbors = c(1, 5)))
})

# ============================================================================
# F. Custom risk/return columns
# ============================================================================

test_that("chart.RiskReward.RP with explicit risk and return cols", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_rp, return.col = "mean", risk.col = "StdDev"))
})

# ============================================================================
# G. Styling
# ============================================================================

test_that("chart.Weight.RP with custom styling and las=1", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Weights(opt_rp, main = "RP Weights", las = 1,
                  element.color = "black")
  )
})
