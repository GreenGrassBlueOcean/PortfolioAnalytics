
require(testthat)
require(PortfolioAnalytics)

context("extractStats and extractObjectiveMeasures")

# ---- Shared test data ----
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# ---- extractStats for random ----

test_that("extractStats works for random optimization", {
  set.seed(4817)
  opt <- optimize.portfolio(R5, portf, optimize_method = "random",
                            search_size = 200, trace = TRUE)
  stats <- extractStats(opt)
  expect_true(is.matrix(stats))
  expect_true(nrow(stats) > 0)
  expect_true(any(grepl("w\\.", colnames(stats))))
})

# ---- extractStats for ROI ----

test_that("extractStats works for ROI optimization", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_roi <- portfolio.spec(assets = colnames(R5))
  portf_roi <- add.constraint(portf_roi, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf_roi <- add.constraint(portf_roi, type = "box", min = 0.05, max = 0.55)
  portf_roi <- add.objective(portf_roi, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R5, portf_roi, optimize_method = "ROI", trace = TRUE)
  stats <- extractStats(opt)
  expect_true(is.matrix(stats) || is.numeric(stats))
})

# ---- extractStats for DEoptim ----

test_that("extractStats works for DEoptim optimization", {
  skip_if_not_installed("DEoptim")

  set.seed(2108)
  opt <- optimize.portfolio(R5, portf, optimize_method = "DEoptim",
                            search_size = 500, trace = TRUE)
  stats <- extractStats(opt)
  expect_true(is.matrix(stats))
  expect_true(nrow(stats) > 0)
})

# ---- extractStats for PSO ----

test_that("extractStats works for PSO optimization", {
  skip_if_not_installed("pso")

  set.seed(7723)
  opt <- optimize.portfolio(R5, portf, optimize_method = "pso", trace = TRUE)
  stats <- extractStats(opt)
  expect_true(is.matrix(stats))
  expect_true(nrow(stats) > 0)
})

# ---- extractStats for GenSA ----

test_that("extractStats works for GenSA optimization", {
  skip_if_not_installed("GenSA")

  set.seed(3491)
  opt <- optimize.portfolio(R5, portf, optimize_method = "GenSA", trace = TRUE)
  stats <- extractStats(opt)
  expect_true(is.numeric(stats))
})

# ---- extractWeights for rebalancing ----

test_that("extractWeights works for rebalancing optimization", {
  set.seed(1573)
  opt_rebal <- optimize.portfolio.rebalancing(R5, portf,
                                              optimize_method = "random",
                                              search_size = 100,
                                              rebalance_on = "quarters",
                                              training_period = 24,
                                              trace = TRUE)
  w <- extractWeights(opt_rebal)
  expect_true(is.xts(w))
  expect_equal(ncol(w), 5)
  expect_true(nrow(w) >= 1)
})

# ---- extractObjectiveMeasures for various types ----

test_that("extractObjectiveMeasures works for random optimization", {
  set.seed(4817)
  opt <- optimize.portfolio(R5, portf, optimize_method = "random",
                            search_size = 200, trace = TRUE)
  obj <- extractObjectiveMeasures(opt)
  expect_true(is.list(obj))
  expect_true("mean" %in% names(obj) || "StdDev" %in% names(obj))
})

test_that("extractObjectiveMeasures works for rebalancing optimization", {
  set.seed(1573)
  opt_rebal <- optimize.portfolio.rebalancing(R5, portf,
                                              optimize_method = "random",
                                              search_size = 100,
                                              rebalance_on = "quarters",
                                              training_period = 24,
                                              trace = TRUE)
  obj <- extractObjectiveMeasures(opt_rebal)
  expect_true(is.xts(obj) || is.data.frame(obj) || is.list(obj))
})

# ---- extractStats for rebalancing ----

test_that("extractStats works for rebalancing optimization", {
  set.seed(1573)
  opt_rebal <- optimize.portfolio.rebalancing(R5, portf,
                                              optimize_method = "random",
                                              search_size = 100,
                                              rebalance_on = "quarters",
                                              training_period = 24,
                                              trace = TRUE)
  stats <- extractStats(opt_rebal)
  expect_true(is.list(stats))
  expect_true(length(stats) >= 1)
})
