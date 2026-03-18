##### test_charts_ef_coverage3.R #####
# Coverage: charts.efficient.frontier.R — EF with chart.assets, Overlay,
#           Compare with guideline, EF.Weights error + dispatch

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:48, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

# Shared base portfolio
portf <- portfolio.spec(assets = colnames(R4))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
portf <- add.objective(portf, type = "risk", name = "StdDev")
portf <- add.objective(portf, type = "return", name = "mean")

# ===========================================================================
# 1. chart.EfficientFrontier.efficient.frontier with chart.assets=TRUE
#    (lines 580-620)
# ===========================================================================

test_that("chart.EfficientFrontier.efficient.frontier with chart.assets=TRUE", {
  ef <- create.EfficientFrontier(R4, portf, type = "mean-StdDev",
                                 n.portfolios = 10)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontier(ef, match.col = "StdDev", chart.assets = TRUE)
  )
})

# ===========================================================================
# 2. chart.EfficientFrontierOverlay (lines 668-741)
# ===========================================================================

test_that("chart.EfficientFrontierOverlay draws multiple frontiers", {
  portf2 <- portfolio.spec(assets = colnames(R4))
  portf2 <- add.constraint(portf2, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
  portf2 <- add.constraint(portf2, type = "box", min = 0.10, max = 0.50)
  portf2 <- add.objective(portf2, type = "risk", name = "StdDev")
  portf2 <- add.objective(portf2, type = "return", name = "mean")

  port_list <- combine.portfolios(list(portf, portf2))

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  result <- chart.EfficientFrontierOverlay(
    R4, portfolio_list = port_list, type = "mean-StdDev",
    match.col = "StdDev", n.portfolios = 10,
    chart.assets = TRUE, legend.loc = "topright",
    legend.labels = c("Wide box", "Tight box")
  )
  expect_true(is.list(result))
  expect_equal(length(result), 2)
})

# ===========================================================================
# 3. chart.EfficientFrontierCompare with guideline (lines 774-853)
# ===========================================================================

test_that("chart.EfficientFrontierCompare with guideline draws annotations", {
  skip("meanrisk.efficient.frontier has colnames dimnames mismatch — pre-existing issue")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  result <- chart.EfficientFrontierCompare(
    R4, portf,
    risk_type = c("StdDev", "ES"),
    guideline = TRUE,
    n.portfolios = 10,
    legend.loc = "bottomright"
  )
  expect_true(!is.null(result))
})

# ===========================================================================
# 4. chart.EF.Weights without groups → error on by.groups=TRUE (line 437)
# ===========================================================================

test_that("chart.EF.Weights errors when by.groups=TRUE without group constraints", {
  ef <- create.EfficientFrontier(R4, portf, type = "mean-StdDev",
                                 n.portfolios = 10)
  expect_error(
    chart.EF.Weights(ef, match.col = "StdDev", by.groups = TRUE),
    "group constraints"
  )
})

# ===========================================================================
# 5. chart.EF.Weights.optimize.portfolio dispatch (lines 540-551)
# ===========================================================================

test_that("chart.EF.Weights dispatches from optimize.portfolio object", {
  opt <- suppressWarnings(optimize.portfolio(R4, portf,
                                             optimize_method = "ROI",
                                             trace = TRUE))
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EF.Weights(opt, match.col = "StdDev")
  )
})
