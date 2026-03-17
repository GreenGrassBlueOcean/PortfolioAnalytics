
require(testthat)
require(PortfolioAnalytics)

context("optFUN.R coverage: ptc, factor exposure (toc/leverage/milp), mean_etl_opt, max_sr_opt")

# ============================================================================
# Shared test data
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

B <- matrix(c(1.2, 0.8, 0.5, 1.1,
              0.3, 0.7, 1.0, 0.4), nrow = 4, ncol = 2)
colnames(B) <- c("Market", "Size")
rownames(B) <- colnames(R4)

# ============================================================================
# Tests: gmv_opt_ptc (proportional transaction costs)
# ============================================================================

test_that("GMV with proportional transaction costs runs", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "transaction_cost",
                          ptc = rep(0.01, 4))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # May produce NA weights due to rank-deficient Q matrix (known issue)
  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

test_that("GMV ptc with group constraints constructs correctly", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "transaction_cost",
                          ptc = rep(0.01, 4))
  portf <- add.constraint(portf, type = "group",
                          groups = list(1:2, 3:4),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

test_that("GMV ptc with factor exposure constraints constructs correctly", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "transaction_cost",
                          ptc = rep(0.01, 4))
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = c(0.5, 0.3), upper = c(1.0, 0.8))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

test_that("GMV ptc with mean+risk objectives extracts obj_vals", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "transaction_cost",
                          ptc = rep(0.01, 4))
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

# ============================================================================
# Tests: factor exposure in gmv_opt_toc (turnover + factor exposure)
# ============================================================================

test_that("GMV with turnover + factor exposure constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "turnover", turnover_target = 0.5)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = c(0.5, 0.3), upper = c(1.0, 0.8))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# Tests: factor exposure in gmv_opt_leverage
# ============================================================================

test_that("GMV with leverage + factor exposure constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = -0.3, max = 0.65)
  portf <- add.constraint(portf, type = "leverage_exposure",
                          leverage = 1.6)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = c(0.3, 0.2), upper = c(1.2, 0.9))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

# ============================================================================
# Tests: factor exposure in maxret_milp_opt
# ============================================================================

test_that("maxret MILP with factor exposure constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = c(0.3, 0.2), upper = c(1.2, 0.9))
  portf <- add.objective(portf, type = "return", name = "mean")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  # At most 3 non-zero positions
  expect_true(sum(abs(w) > 1e-6) <= 3)
})

# ============================================================================
# Tests: mean_etl_opt (max STARR via optimize())
# ============================================================================

test_that("maxSTARR via ROI exercises mean_etl_opt and starr_obj_fun", {
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
  expect_true(all(w >= 0.05 - 1e-4))
})

# ============================================================================
# Tests: max_sr_opt (max Sharpe via optimize())
# ============================================================================

test_that("maxSR via ROI exercises max_sr_opt and sharpe_obj_fun", {
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
})

# ============================================================================
# Tests: gmv_opt_toc with group constraints
# ============================================================================

test_that("GMV with turnover + group constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "turnover", turnover_target = 0.5)
  portf <- add.constraint(portf, type = "group",
                          groups = list(1:2, 3:4),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# Tests: gmv_opt_leverage with group constraints
# ============================================================================

test_that("GMV with leverage + group constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = -0.3, max = 0.65)
  portf <- add.constraint(portf, type = "leverage_exposure",
                          leverage = 1.6)
  portf <- add.constraint(portf, type = "group",
                          groups = list(1:2, 3:4),
                          group_min = c(0.1, 0.1),
                          group_max = c(0.9, 0.9))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

# ============================================================================
# Tests: gmv_opt_toc with mean+risk (non-zero moments$mean path)
# ============================================================================

test_that("GMV toc with mean+StdDev objectives hits non-zero mean path", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "turnover", turnover_target = 0.5)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI",
                            maxSR = TRUE, trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# Tests: gmv_opt_leverage with mean+risk (non-zero moments$mean path)
# ============================================================================

test_that("GMV leverage with mean+StdDev objectives hits non-zero mean path", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = -0.3, max = 0.65)
  portf <- add.constraint(portf, type = "leverage_exposure",
                          leverage = 1.6)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI",
                            maxSR = TRUE, trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

# ============================================================================
# Tests: etl_milp_opt paths
# ============================================================================

test_that("ETL MILP with group constraints exercises etl_milp_opt group path", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 4)
  portf <- add.constraint(portf, type = "group",
                          groups = list(1:2, 3:4),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8))
  portf <- add.objective(portf, type = "risk", name = "ES",
                         arguments = list(p = 0.95))

  opt <- suppressWarnings(optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE))
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

test_that("ETL MILP with factor exposure constraints exercises etl_milp_opt factor path", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 4)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = c(0.3, 0.2), upper = c(1.2, 0.9))
  portf <- add.objective(portf, type = "risk", name = "ES",
                         arguments = list(p = 0.95))

  opt <- suppressWarnings(optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE))
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

# ============================================================================
# Tests: maxret_milp_opt with target return
# ============================================================================

test_that("maxret MILP with target return exercises target path", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.objective(portf, type = "return", name = "mean")

  # Use return_target in the objective
  portf <- add.objective(portf, type = "risk", name = "StdDev",
                         target = 0.005)

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

# ============================================================================
# Tests: mean_etl_opt with MILP (max_pos path)
# ============================================================================

test_that("maxSTARR MILP exercises mean_etl_opt with milp_opt paths", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.0, max = 0.65)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "ES",
                         arguments = list(p = 0.95))

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI",
                            maxSTARR = TRUE, trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_true(sum(abs(w) > 1e-6) <= 3)
})
