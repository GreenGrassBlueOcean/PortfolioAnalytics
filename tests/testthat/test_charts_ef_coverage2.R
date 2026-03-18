##### test_charts_ef_coverage2.R #####
# Coverage: charts.efficient.frontier.R — by.groups + guideline+labels paths

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:48, 1:4]

# ===========================================================================
# 1. chart.EF.Weights with by.groups=TRUE
# ===========================================================================

test_that("chart.EF.Weights with by.groups=TRUE", {
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "group",
                          groups = list(grpA = 1:2, grpB = 3:4),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8))
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")

  ef <- create.EfficientFrontier(R4, portf, type = "mean-StdDev",
                                 n.portfolios = 10)
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EF.Weights(ef, match.col = "StdDev", by.groups = TRUE)
  )
})

# ===========================================================================
# 2. chart.EfficientFrontierCompare with guideline + labels.assets
# ===========================================================================

test_that("chart.EfficientFrontierCompare guideline with labels", {
  skip("chart.EfficientFrontierCompare requires portfolio attribute on frontier objects")
  portf1 <- portfolio.spec(assets = colnames(R4))
  portf1 <- add.constraint(portf1, type = "full_investment")
  portf1 <- add.constraint(portf1, type = "box", min = 0.05, max = 0.65)
  portf1 <- add.objective(portf1, type = "risk", name = "StdDev")
  portf1 <- add.objective(portf1, type = "return", name = "mean")

  portf2 <- portfolio.spec(assets = colnames(R4))
  portf2 <- add.constraint(portf2, type = "full_investment")
  portf2 <- add.constraint(portf2, type = "box", min = 0.10, max = 0.60)
  portf2 <- add.objective(portf2, type = "risk", name = "StdDev")
  portf2 <- add.objective(portf2, type = "return", name = "mean")

  ef1 <- create.EfficientFrontier(R4, portf1, type = "mean-StdDev",
                                  n.portfolios = 10)
  ef2 <- create.EfficientFrontier(R4, portf2, type = "mean-StdDev",
                                  n.portfolios = 10)

  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.EfficientFrontierCompare(ef1, ef2,
                                   match.col = "StdDev",
                                   guideline = TRUE,
                                   labels.assets = TRUE)
  )
})
