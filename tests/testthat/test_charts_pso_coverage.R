context("charts.PSO.R coverage: Weight, Scatter, plot for PSO")


skip_if_not_installed("pso")

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "risk", name = "ES")
portf <- add.objective(portf, type = "return", name = "mean")

set.seed(4729)
opt_pso <- suppressWarnings(
  optimize.portfolio(R5, portf, optimize_method = "pso",
                     search_size = 500, trace = TRUE)
)

# ============================================================================
# A. chart.Weight.pso — line plot
# ============================================================================

test_that("chart.Weight.pso line plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_pso))
})

# ============================================================================
# B. chart.Weight.pso — bar plot
# ============================================================================

test_that("chart.Weight.pso bar plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_pso, plot.type = "bar"))
})

# ============================================================================
# C. chart.Scatter.pso / chart.RiskReward
# ============================================================================

test_that("chart.RiskReward.pso scatter plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso))
})

test_that("chart.RiskReward.pso with chart.assets=TRUE", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso, chart.assets = TRUE))
})

test_that("chart.RiskReward.pso with custom return.col and risk.col", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso, return.col = "mean", risk.col = "ES"))
})

# ============================================================================
# D. plot.optimize.portfolio.pso (combined layout)
# ============================================================================

test_that("plot method for PSO optimization works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(opt_pso))
})

# ============================================================================
# E. Neighbors parameter variations
# ============================================================================

test_that("chart.RiskReward.pso with neighbors as integer", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso, neighbors = 3))
})

test_that("chart.RiskReward.pso with neighbors as vector", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_pso, neighbors = c(1, 3)))
})

# ============================================================================
# F. Styling parameters
# ============================================================================

test_that("chart.Weight.pso with custom styling", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Weights(opt_pso, main = "PSO Weights", las = 1,
                  element.color = "black", cex.axis = 1.0)
  )
})
