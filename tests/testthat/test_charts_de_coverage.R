context("charts.DE.R coverage: Weight, Scatter, trajectory, plot for DEoptim")


skip_if_not_installed("DEoptim")

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "risk", name = "ES")
portf <- add.objective(portf, type = "return", name = "mean")

set.seed(3812)
opt_de <- suppressWarnings(suppressMessages(
  optimize.portfolio(R5, portf, optimize_method = "DEoptim",
                     search_size = 500, trace = TRUE,
                     DEoptim.control = list(itermax = 25, trace = FALSE))
))

# ============================================================================
# A. chart.Weight.DE — line plot
# ============================================================================

test_that("chart.Weight.DE line plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_de))
})

# ============================================================================
# B. chart.Weight.DE — bar plot
# ============================================================================

test_that("chart.Weight.DE bar plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_de, plot.type = "bar"))
})

# ============================================================================
# C. chart.Scatter.DE / chart.RiskReward
# ============================================================================

test_that("chart.RiskReward.DE scatter plot works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_de))
})

test_that("chart.RiskReward.DE with chart.assets=TRUE", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_de, chart.assets = TRUE))
})

# ============================================================================
# D. plot.optimize.portfolio.DEoptim (combined layout with trajectory)
# ============================================================================

test_that("plot method for DE optimization works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(plot(opt_de))
})

# ============================================================================
# E. Neighbors parameter
# ============================================================================

test_that("chart.RiskReward.DE with xlim and ylim", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_de, xlim = c(0, 0.1), ylim = c(0, 0.02)))
})

# ============================================================================
# F. Custom risk and return columns
# ============================================================================

test_that("chart.RiskReward.DE with explicit risk and return cols", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.RiskReward(opt_de, return.col = "mean", risk.col = "ES"))
})

# ============================================================================
# G. Styling parameters
# ============================================================================

test_that("chart.Weight.DE with custom styling", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Weights(opt_de, main = "DE Weights", las = 1,
                  cex.axis = 1.0, element.color = "black")
  )
})
