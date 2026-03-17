context("chart.concentration.R coverage: chart.Concentration additional paths")

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

set.seed(7263)
opt <- optimize.portfolio(R5, portf, optimize_method = "random",
                          search_size = 500, trace = TRUE)

# Risk budget portfolio for pct_contrib
portf_rb <- portfolio.spec(assets = colnames(R5))
portf_rb <- add.constraint(portf_rb, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
portf_rb <- add.constraint(portf_rb, type = "box", min = 0.05, max = 0.55)
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "StdDev",
                          max_prisk = 0.5)

set.seed(4581)
opt_rb <- optimize.portfolio(R5, portf_rb, optimize_method = "random",
                             search_size = 500, trace = TRUE)

# ============================================================================
# A. chart.Concentration with conc.type = "weights" (default)
# ============================================================================

test_that("chart.Concentration works with weights concentration", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Concentration(opt, conc.type = "weights"))
})

# ============================================================================
# B. chart.Concentration with chart.assets = TRUE
# ============================================================================

test_that("chart.Concentration with chart.assets=TRUE", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Concentration(opt, conc.type = "weights", chart.assets = TRUE)
  )
})

# ============================================================================
# C. chart.Concentration with conc.type = "pct_contrib"
# ============================================================================

test_that("chart.Concentration with pct_contrib type", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Concentration(opt_rb, conc.type = "pct_contrib")
  )
})

# ============================================================================
# D. chart.Concentration with custom colors
# ============================================================================

test_that("chart.Concentration with custom color palette", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Concentration(opt, conc.type = "weights",
                        col = heat.colors(15))
  )
})

# ============================================================================
# E. chart.Concentration with custom axis limits
# ============================================================================

test_that("chart.Concentration with custom xlim and ylim", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.Concentration(opt, conc.type = "weights",
                        xlim = c(0, 0.1), ylim = c(0, 0.02))
  )
})

# ============================================================================
# F. Error handling
# ============================================================================

test_that("chart.Concentration errors on invalid object", {
  expect_error(chart.Concentration(list()), "optimize.portfolio")
})
