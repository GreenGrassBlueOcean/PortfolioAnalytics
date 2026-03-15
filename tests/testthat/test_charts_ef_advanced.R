##### test_charts_ef_advanced.R #####
# Phase 3B: chart.EfficientFrontier advanced paths
# Targets uncovered code in charts.efficient.frontier.R:
#   - chart.EfficientFrontier.optimize.portfolio.CVXR: StdDev, ES, CSM match.col
#   - chart.EfficientFrontier.optimize.portfolio.ROI: ETL match.col path
#   - chart.EfficientFrontier.efficient.frontier: rf=NULL, chart.assets=FALSE
#   - chart.EF.Weights.efficient.frontier: legend.loc=NULL
#   - chart.EfficientFrontierCompare: mean-risk guideline comparison
#   - chart.EfficientFrontierOverlay: with legend, chart.assets=FALSE

library(testthat)
library(PortfolioAnalytics)

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# ===========================================================================
# A. chart.EfficientFrontier.optimize.portfolio.CVXR
# ===========================================================================

test_that("CVXR chart.EfficientFrontier runs with match.col='StdDev'", {
  skip_if_not_installed("CVXR")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt_cvxr <- optimize.portfolio(R5, portf, optimize_method = "CVXR",
                                 trace = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(opt_cvxr, match.col = "StdDev", n.portfolios = 10)
  )
})

test_that("CVXR chart.EfficientFrontier runs with match.col='ES'", {
  skip_if_not_installed("CVXR")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "ES")

  opt_cvxr <- optimize.portfolio(R5, portf, optimize_method = "CVXR",
                                 trace = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(opt_cvxr, match.col = "ES", n.portfolios = 10)
  )
})

test_that("CVXR chart.EfficientFrontier with rf=NULL omits tangent line", {
  skip_if_not_installed("CVXR")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt_cvxr <- optimize.portfolio(R5, portf, optimize_method = "CVXR",
                                 trace = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(opt_cvxr, match.col = "StdDev",
                            n.portfolios = 10, rf = NULL)
  )
})

test_that("CVXR chart.EfficientFrontier with chart.assets=FALSE", {
  skip_if_not_installed("CVXR")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt_cvxr <- optimize.portfolio(R5, portf, optimize_method = "CVXR",
                                 trace = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(opt_cvxr, match.col = "StdDev",
                            n.portfolios = 10, chart.assets = FALSE)
  )
})

test_that("CVXR chart.EfficientFrontier with CSM match.col errors on scatterFUN", {
  skip_if_not_installed("CVXR")

  # CSM is a CVXR-only risk measure with no standalone function,
  # so scatterFUN (which uses match.fun) cannot find it.
  # This is a known limitation: chart.EF dispatches to scatterFUN
  # for asset scatter but CSM is not in the switch/match.fun table.
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "CSM")

  opt_cvxr <- suppressWarnings(
    optimize.portfolio(R5, portf, optimize_method = "CVXR", trace = TRUE)
  )
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_error(
    chart.EfficientFrontier(opt_cvxr, match.col = "CSM", n.portfolios = 10),
    "CSM"
  )
})

# ===========================================================================
# B. chart.EfficientFrontier.optimize.portfolio.ROI — ETL path
# ===========================================================================

test_that("ROI chart.EfficientFrontier with match.col='ES' (ETL path)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "ES")

  opt_roi <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                                trace = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(opt_roi, match.col = "ES", n.portfolios = 10)
  )
})

test_that("ROI chart.EfficientFrontier with rf=NULL", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt_roi <- optimize.portfolio(R5, portf, optimize_method = "ROI",
                                trace = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(opt_roi, match.col = "StdDev",
                            n.portfolios = 10, rf = NULL)
  )
})

# ===========================================================================
# C. chart.EfficientFrontier.efficient.frontier
# ===========================================================================

test_that("chart.EF for efficient.frontier object with rf=NULL", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  ef <- create.EfficientFrontier(R5, portf, type = "mean-StdDev",
                                 n.portfolios = 10)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(ef, match.col = "StdDev", rf = NULL)
  )
})

test_that("chart.EF for efficient.frontier object with chart.assets=FALSE", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  ef <- create.EfficientFrontier(R5, portf, type = "mean-StdDev",
                                 n.portfolios = 10)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(ef, match.col = "StdDev", chart.assets = FALSE)
  )
})

# ===========================================================================
# D. chart.EF.Weights — legend.loc=NULL
# ===========================================================================

test_that("chart.EF.Weights with legend.loc=NULL omits legend", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  ef <- create.EfficientFrontier(R5, portf, type = "mean-StdDev",
                                 n.portfolios = 10)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EF.Weights(ef, match.col = "StdDev", legend.loc = NULL)
  )
})

# ===========================================================================
# E. chart.EfficientFrontier.optimize.portfolio (generic — DE/random based)
# ===========================================================================

test_that("chart.EF for random optimization with rf=NULL", {
  set.seed(3792)
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt_rp <- optimize.portfolio(R5, portf, optimize_method = "random",
                               search_size = 500, trace = TRUE)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(opt_rp, match.col = "StdDev", rf = NULL)
  )
})

test_that("chart.EF for GenSA optimization errors (no trace)", {
  skip_if_not_installed("GenSA")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  set.seed(5192)
  opt_gs <- suppressWarnings(
    optimize.portfolio(R5, portf, optimize_method = "GenSA", trace = TRUE)
  )
  expect_error(
    chart.EfficientFrontier(opt_gs, match.col = "StdDev"),
    "GenSA does not return"
  )
})

# ===========================================================================
# F. chart.EfficientFrontierCompare
# ===========================================================================

test_that("chart.EfficientFrontierCompare with guidelines runs", {
  skip_if_not_installed("CVXR")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "risk", name = "ES")

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontierCompare(
      R5, portf, risk_type = "StdDev",
      match.col = c("StdDev", "ES"),
      n.portfolios = 10,
      legend.loc = "bottomright"
    )
  )
})

test_that("chart.EfficientFrontierCompare with guideline=FALSE runs", {
  skip_if_not_installed("CVXR")

  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "risk", name = "ES")

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontierCompare(
      R5, portf, risk_type = "StdDev",
      match.col = c("StdDev", "ES"),
      n.portfolios = 10,
      guideline = FALSE
    )
  )
})

# ===========================================================================
# G. chart.EfficientFrontierOverlay with chart.assets=FALSE and legend
# ===========================================================================

test_that("chart.EfficientFrontierOverlay with chart.assets=FALSE and legend", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  p1 <- portfolio.spec(assets = colnames(R5))
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.constraint(p1, type = "box", min = 0.05, max = 0.55)
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- portfolio.spec(assets = colnames(R5))
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.constraint(p2, type = "box", min = 0.10, max = 0.40)
  p2 <- add.objective(p2, type = "risk", name = "StdDev")

  plist <- combine.portfolios(list(p1, p2))

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontierOverlay(
      R5, portfolio_list = plist, type = "mean-StdDev",
      n.portfolios = 10, match.col = "StdDev",
      chart.assets = FALSE,
      legend.loc = "topleft",
      legend.labels = c("Wide", "Narrow")
    )
  )
})

# ===========================================================================
# H. Class validation
# ===========================================================================

test_that("chart.EF.Weights rejects non-efficient.frontier class", {
  mock <- list(weights = 1:5)
  class(mock) <- "not_ef"
  expect_error(chart.EF.Weights(mock), "no applicable method")
})
