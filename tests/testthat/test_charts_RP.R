library(testthat)
library(PortfolioAnalytics)

context("charts.RP: Weight, Scatter (RiskReward), and plot dispatches")

data(edhec)
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# Shared optimization result
set.seed(6371)
opt_rp <- optimize.portfolio(R5, portf, optimize_method = "random",
                             search_size = 500, trace = TRUE)

# ============================================================================
# A. chart.Weights (line plot — default)
# ============================================================================

test_that("chart.Weight.RP line plot runs without error", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.Weights(opt_rp), NA)
})

# ============================================================================
# B. chart.Weights (barplot)
# ============================================================================

test_that("chart.Weight.RP barplot runs without error", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(chart.Weights(opt_rp, plot.type = "bar"), NA)
})

# ============================================================================
# C. chart.RiskReward (scatter)
# ============================================================================

test_that("chart.Scatter.RP runs for mean-StdDev", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.RiskReward(opt_rp, return.col = "mean", risk.col = "StdDev"),
    NA
  )
})

test_that("chart.Scatter.RP runs with chart.assets=TRUE", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.RiskReward(opt_rp, return.col = "mean", risk.col = "StdDev",
                     chart.assets = TRUE),
    NA
  )
})

# ============================================================================
# D. chart.Scatter.RP with neighbors
# ============================================================================

test_that("chart.Scatter.RP runs with numeric neighbors", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.RiskReward(opt_rp, return.col = "mean", risk.col = "StdDev",
                     neighbors = 5),
    NA
  )
})

# ============================================================================
# E. plot.optimize.portfolio.random
# ============================================================================

test_that("plot.optimize.portfolio.random runs without error", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    plot(opt_rp, return.col = "mean", risk.col = "StdDev"),
    NA
  )
})

# ============================================================================
# F. chart.Weight.RP with custom xlim, main
# ============================================================================

test_that("chart.Weight.RP accepts custom main and xlab", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.Weights(opt_rp, main = "Custom Title", xlab = "Assets"),
    NA
  )
})

# ============================================================================
# G. chart.Scatter.RP with custom xlim/ylim
# ============================================================================

test_that("chart.Scatter.RP accepts custom axis limits", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.RiskReward(opt_rp, return.col = "mean", risk.col = "StdDev",
                     xlim = c(0, 0.05), ylim = c(0, 0.01)),
    NA
  )
})
