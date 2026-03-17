context("plotFrontiers coverage")

library(testthat)
library(PortfolioAnalytics)

skip_if_not_installed("CVXR")
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:48, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

# Build a simple portfolio for efficient frontiers
mv_portf <- portfolio.spec(assets = colnames(R4))
mv_portf <- add.constraint(mv_portf, type = "full_investment")
mv_portf <- add.constraint(mv_portf, type = "box", min = 0.05, max = 0.6)
mv_portf <- add.objective(mv_portf, type = "risk", name = "var")
mv_portf <- add.objective(mv_portf, type = "return", name = "mean")

# Generate two frontiers with slightly different constraints
mv_portf2 <- portfolio.spec(assets = colnames(R4))
mv_portf2 <- add.constraint(mv_portf2, type = "full_investment")
mv_portf2 <- add.constraint(mv_portf2, type = "box", min = 0.1, max = 0.5)
mv_portf2 <- add.objective(mv_portf2, type = "risk", name = "var")
mv_portf2 <- add.objective(mv_portf2, type = "return", name = "mean")

ef1 <- meanvar.efficient.frontier(mv_portf, R4,
                                   optimize_method = "CVXR",
                                   n.portfolios = 5)
ef2 <- meanvar.efficient.frontier(mv_portf2, R4,
                                   optimize_method = "CVXR",
                                   n.portfolios = 5)

# ============================================================================
# A. Basic plotFrontiers call with risk="StdDev"
# ============================================================================

test_that("plotFrontiers works with StdDev risk type", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1, ef2), risk = "StdDev")
  expect_type(res, "list")
  expect_named(res, c("mean", "risk"))
  expect_equal(length(res$mean), 2)
  expect_equal(length(res$risk), 2)
})

# ============================================================================
# B. Risk type aliases
# ============================================================================

test_that("plotFrontiers accepts 'var' alias for StdDev", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1), risk = "var")
  expect_type(res, "list")
})

test_that("plotFrontiers accepts 'stdDev' alias for StdDev", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1), risk = "stdDev")
  expect_type(res, "list")
})

# ============================================================================
# C. Custom xlim, ylim, col, lty, lwd
# ============================================================================

test_that("plotFrontiers respects custom xlim and ylim", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1, ef2), risk = "StdDev",
                       xlim = c(0, 0.1), ylim = c(0, 0.02))
  expect_type(res, "list")
})

test_that("plotFrontiers respects custom col, lty, lwd", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1, ef2), risk = "StdDev",
                       col = c("red", "blue"), lty = c(1, 2), lwd = c(2, 3))
  expect_type(res, "list")
})

# ============================================================================
# D. Legend options
# ============================================================================

test_that("plotFrontiers handles legend.loc and legend.labels", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1, ef2), risk = "StdDev",
                       legend.loc = "topright",
                       legend.labels = c("Broad", "Narrow"))
  expect_type(res, "list")
})

test_that("plotFrontiers generates default legend labels", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1, ef2), risk = "StdDev",
                       legend.loc = "bottomright")
  expect_type(res, "list")
})

# ============================================================================
# E. Single frontier
# ============================================================================

test_that("plotFrontiers works with a single frontier", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1), risk = "StdDev")
  expect_type(res, "list")
  expect_equal(length(res$mean), 1)
})

# ============================================================================
# F. plot_type parameter
# ============================================================================

test_that("plotFrontiers works with plot_type='b' (both points and lines)", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1), risk = "StdDev",
                       plot_type = "b")
  expect_type(res, "list")
})

# ============================================================================
# G. Return value structure
# ============================================================================

test_that("plotFrontiers returns invisible list with correct structure", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- plotFrontiers(R4, frontiers = list(ef1, ef2), risk = "StdDev")
  
  expect_type(res, "list")
  expect_named(res, c("mean", "risk"))
  # Each element should be a list of numeric vectors
  expect_true(is.numeric(res$mean[[1]]))
  expect_true(is.numeric(res$risk[[1]]))
  expect_equal(length(res$mean[[1]]), nrow(ef1))
  expect_equal(length(res$risk[[1]]), nrow(ef1))
})
