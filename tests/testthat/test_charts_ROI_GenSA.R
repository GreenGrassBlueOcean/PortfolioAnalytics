##### test_charts_ROI_GenSA.R #####
# Chart dispatch coverage for charts.ROI.R and charts.GenSA.R
# Targets: chart.Weight.ROI / chart.Weight.GenSA (line + barplot),
#          chart.Scatter.ROI / chart.Scatter.GenSA (risk-reward scatter),
#          .charts_ROI / .charts_GenSA (combined plot method),
#          class validation, missing-R error path, chart.assets overlay,
#          margin branches (main="", xlab=, las=1), infinite-bound ylim

library(testthat)
library(PortfolioAnalytics)

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "full_investment")
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# Portfolio with long_only (min=0, max=Inf) to trigger infinite-bound branch
portf_inf <- portfolio.spec(assets = colnames(R5))
portf_inf <- add.constraint(portf_inf, type = "full_investment")
portf_inf <- add.constraint(portf_inf, type = "long_only")
portf_inf <- add.objective(portf_inf, type = "risk", name = "StdDev")

# ===========================================================================
# A. ROI chart dispatch
# ===========================================================================

# Pre-compute ROI results once
opt_roi <- local({
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  optimize.portfolio(R5, portf, optimize_method = "ROI", trace = TRUE)
})

opt_roi_inf <- local({
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  optimize.portfolio(R5, portf_inf, optimize_method = "ROI", trace = TRUE)
})

test_that("ROI chart.Weights (line) runs without error", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_roi))
})

test_that("ROI chart.Weights (barplot) runs without error", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_roi, plot.type = "bar"))
})

test_that("ROI chart.Weights with main='' suppresses top margin", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_roi, main = ""))
})

test_that("ROI chart.Weights with xlab sets wider bottom margin", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_roi, xlab = "Assets"))
})

test_that("ROI chart.Weights with las=1 uses minimal bottom margin", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_roi, las = 1))
})

test_that("ROI chart.Weights with infinite box constraints uses weight-based ylim", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_roi_inf))
})

test_that("ROI chart.RiskReward runs without error", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_roi, return.col = "mean", risk.col = "StdDev")
  )
})

test_that("ROI chart.RiskReward with chart.assets=TRUE runs without error", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_roi, return.col = "mean", risk.col = "StdDev",
                     chart.assets = TRUE)
  )
})

test_that("ROI plot method (combined layout) runs without error", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    plot(opt_roi, return.col = "mean", risk.col = "StdDev")
  )
})

# ---------------------------------------------------------------------------
# ROI class validation and error paths
# ---------------------------------------------------------------------------

test_that("chart.Weight.ROI rejects wrong class", {
  mock <- list(weights = rep(0.2, 5))
  class(mock) <- "optimize.portfolio.random"
  expect_error(
    PortfolioAnalytics:::chart.Weight.ROI(mock),
    "optimize.portfolio.ROI"
  )
})

test_that("chart.Scatter.ROI rejects wrong class", {
  mock <- list(weights = rep(0.2, 5))
  class(mock) <- "optimize.portfolio.random"
  expect_error(
    PortfolioAnalytics:::chart.Scatter.ROI(mock),
    "optimize.portfolio.ROI"
  )
})

test_that("chart.Scatter.ROI errors when R is missing (trace=FALSE)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  obj <- opt_roi
  obj$R <- NULL
  expect_error(
    PortfolioAnalytics:::chart.Scatter.ROI(obj),
    "Returns object not detected"
  )
})

# ===========================================================================
# B. GenSA chart dispatch
# ===========================================================================

# Pre-compute GenSA results once
opt_gensa <- local({
  skip_if_not_installed("GenSA")
  set.seed(5261)
  suppressWarnings(
    optimize.portfolio(R5, portf, optimize_method = "GenSA", trace = TRUE)
  )
})

opt_gensa_inf <- local({
  skip_if_not_installed("GenSA")
  set.seed(5261)
  suppressWarnings(
    optimize.portfolio(R5, portf_inf, optimize_method = "GenSA", trace = TRUE)
  )
})

test_that("GenSA chart.Weights (line) runs without error", {
  skip_if_not_installed("GenSA")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_gensa))
})

test_that("GenSA chart.Weights (barplot) runs without error", {
  skip_if_not_installed("GenSA")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_gensa, plot.type = "bar"))
})

test_that("GenSA chart.Weights with main='' suppresses top margin", {
  skip_if_not_installed("GenSA")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_gensa, main = ""))
})

test_that("GenSA chart.Weights with infinite box constraints uses weight-based ylim", {
  skip_if_not_installed("GenSA")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.Weights(opt_gensa_inf))
})

test_that("GenSA chart.RiskReward runs without error", {
  skip_if_not_installed("GenSA")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_gensa, return.col = "mean", risk.col = "StdDev")
  )
})

test_that("GenSA chart.RiskReward with chart.assets=TRUE runs without error", {
  skip_if_not_installed("GenSA")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskReward(opt_gensa, return.col = "mean", risk.col = "StdDev",
                     chart.assets = TRUE)
  )
})

test_that("GenSA plot method (combined layout) runs without error", {
  skip_if_not_installed("GenSA")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    plot(opt_gensa, return.col = "mean", risk.col = "StdDev")
  )
})

# ---------------------------------------------------------------------------
# GenSA class validation and error paths
# ---------------------------------------------------------------------------

test_that("chart.Weight.GenSA rejects wrong class", {
  mock <- list(weights = rep(0.2, 5))
  class(mock) <- "optimize.portfolio.ROI"
  expect_error(
    PortfolioAnalytics:::chart.Weight.GenSA(mock),
    "optimize.portfolio.GenSA"
  )
})

test_that("chart.Scatter.GenSA rejects wrong class", {
  mock <- list(weights = rep(0.2, 5))
  class(mock) <- "optimize.portfolio.ROI"
  expect_error(
    PortfolioAnalytics:::chart.Scatter.GenSA(mock),
    "optimize.portfolio.GenSA"
  )
})

test_that("chart.Scatter.GenSA errors when R is missing", {
  skip_if_not_installed("GenSA")
  obj <- opt_gensa
  obj$R <- NULL
  expect_error(
    PortfolioAnalytics:::chart.Scatter.GenSA(obj),
    "Returns object not detected"
  )
})

# ===========================================================================
# C. Alias identity checks
# ===========================================================================

test_that("chart.Weights S3 method aliases are correct", {
  expect_identical(
    PortfolioAnalytics:::chart.Weights.optimize.portfolio.ROI,
    PortfolioAnalytics:::chart.Weight.ROI
  )
  expect_identical(
    PortfolioAnalytics:::chart.Weights.optimize.portfolio.GenSA,
    PortfolioAnalytics:::chart.Weight.GenSA
  )
})

test_that("chart.RiskReward S3 method aliases are correct", {
  expect_identical(
    PortfolioAnalytics:::chart.RiskReward.optimize.portfolio.ROI,
    PortfolioAnalytics:::chart.Scatter.ROI
  )
  expect_identical(
    PortfolioAnalytics:::chart.RiskReward.optimize.portfolio.GenSA,
    PortfolioAnalytics:::chart.Scatter.GenSA
  )
})
