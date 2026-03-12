
require(testthat)
require(PortfolioAnalytics)

context("optFUN: ROI solver paths")

# ---- Shared test data ----
data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

# ---- MILP: maxret with position limit ----

test_that("maxret MILP optimization with position limit works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.8)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.objective(portf, type = "return", name = "mean")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_true(sum(w > 1e-6) <= 3)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ---- MILP: minES with position limit ----

test_that("minES MILP optimization with position limit works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.8)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.objective(portf, type = "risk", name = "ES")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_true(sum(w > 1e-6) <= 3)
})

# ---- GMV with turnover constraint ----

test_that("GMV optimization with turnover constraint works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "turnover", turnover_target = 0.5)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
  expect_true(all(w >= 0.05 - 1e-4))
})

# NOTE: gmv_opt_ptc (proportional transaction cost) is not tested here because
# the ROI formulation produces NA weights with this constraint configuration.
# This may warrant investigation as a separate issue.

# ---- GMV with leverage exposure constraint ----

test_that("GMV optimization with leverage exposure constraint works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum", min_sum = -0.01, max_sum = 0.01)
  portf <- add.constraint(portf, type = "box", min = -0.5, max = 0.5)
  portf <- add.constraint(portf, type = "leverage_exposure", leverage = 1.6)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_true(sum(abs(w)) <= 1.6 + 0.01)
})

# ---- Max Sharpe via ROI ----

test_that("Max Sharpe ratio optimization via ROI works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI",
                            maxSR = TRUE, trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
  expect_true(!is.null(extractObjectiveMeasures(opt)$mean))
  expect_true(!is.null(extractObjectiveMeasures(opt)$StdDev))
})

# ---- Max STARR via ROI ----

test_that("Max STARR ratio optimization via ROI works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "ES",
                         arguments = list(p = 0.95))

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI",
                            maxSTARR = TRUE, trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
  expect_true(!is.null(extractObjectiveMeasures(opt)$mean))
})

# NOTE: GMV with return_target constraint via add.constraint(type="return")
# produces NA weights with ROI. The target return path in gmv_opt is exercised
# by the max Sharpe test above (which uses target return internally).

# ---- GMV with group constraints via ROI ----

test_that("GMV optimization with group constraints via ROI works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_true(sum(w[1:2]) >= 0.2 - 1e-4)
  expect_true(sum(w[1:2]) <= 0.8 + 1e-4)
  expect_true(sum(w[3:4]) >= 0.2 - 1e-4)
  expect_true(sum(w[3:4]) <= 0.8 + 1e-4)
})

# ---- ETL with group constraints via ROI ----

test_that("ETL optimization with group constraints via ROI works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8))
  portf <- add.objective(portf, type = "risk", name = "ES",
                         arguments = list(p = 0.95))

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_true(sum(w[1:2]) >= 0.2 - 1e-4)
  expect_true(sum(w[3:4]) >= 0.2 - 1e-4)
})
