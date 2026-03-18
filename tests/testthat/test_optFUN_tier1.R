context("optFUN coverage: group, factor exposure, concentration, MILP, turnover, leverage")


skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("ROI.plugin.glpk")

data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# ============================================================================
# A. GMV with group constraints (covers lines 69-80 in gmv_opt)
# ============================================================================

test_that("gmv_opt handles group constraints via ROI", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4, 5)),
                          group_min = c(0.2, 0.3),
                          group_max = c(0.5, 0.7))
  portf <- add.objective(portf, type = "risk", name = "var")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  
  expect_equal(length(w), 5)
  expect_equal(sum(w), 1, tolerance = 0.02)
  # Group constraints respected
  expect_true(sum(w[1:2]) >= 0.2 - 1e-4)
  expect_true(sum(w[1:2]) <= 0.5 + 1e-4)
  expect_true(sum(w[3:5]) >= 0.3 - 1e-4)
  expect_true(sum(w[3:5]) <= 0.7 + 1e-4)
})

# ============================================================================
# B. GMV with factor exposure constraints (covers lines 83-88 in gmv_opt)
# ============================================================================

test_that("gmv_opt handles factor exposure constraints via ROI", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  
  # Factor exposure: sum of weights * factor loading between bounds
  B <- matrix(c(1.2, 0.8, 1.0, 0.9, 1.1), ncol = 1)
  rownames(B) <- colnames(R5)
  colnames(B) <- "market"
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.9, upper = 1.1)
  portf <- add.objective(portf, type = "risk", name = "var")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  
  expect_equal(length(w), 5)
  exposure <- sum(w * B[, 1])
  expect_true(exposure >= 0.9 - 1e-4)
  expect_true(exposure <= 1.1 + 1e-4)
})

# ============================================================================
# C. GMV with group + factor exposure combined
# ============================================================================

test_that("gmv_opt handles group + factor exposure constraints together", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2, 3), c(4, 5)),
                          group_min = c(0.3, 0.2),
                          group_max = c(0.7, 0.5))
  B <- matrix(rep(1, 5), ncol = 1)
  rownames(B) <- colnames(R5)
  colnames(B) <- "ones"
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.95, upper = 1.05)
  portf <- add.objective(portf, type = "risk", name = "var")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# D. Quadratic utility with concentration aversion (lambda_hhi, no groups)
#    (covers lines 103-105 in gmv_opt)
# ============================================================================

test_that("gmv_opt handles concentration aversion (lambda_hhi scalar)", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "var")
  portf <- add.objective(portf, type = "weight_concentration",
                         name = "HHI", conc_aversion = 0.1)
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_equal(length(w), 5)
  expect_equal(sum(w), 1, tolerance = 0.02)
  # With concentration aversion, weights should be more spread out
  expect_true(max(w) < 0.56)
})

# ============================================================================
# E. Concentration aversion with conc_groups (covers lines 108-124 in gmv_opt)
# ============================================================================

test_that("gmv_opt handles concentration aversion with conc_groups", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "risk", name = "var")
  portf <- add.objective(portf, type = "weight_concentration",
                         name = "HHI", conc_aversion = c(0.1, 0.2),
                         conc_groups = list(c(1, 2), c(3, 4, 5)))
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_equal(length(w), 5)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# F. Max return with group constraints (covers lines 216-227 in maxret_opt)
# ============================================================================

test_that("maxret_opt handles group constraints via ROI", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4, 5)),
                          group_min = c(0.2, 0.3),
                          group_max = c(0.5, 0.7))
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_true(sum(w[1:2]) >= 0.2 - 1e-4)
  expect_true(sum(w[3:5]) >= 0.3 - 1e-4)
})

# ============================================================================
# G. Max return with factor exposure constraints (covers lines 230-235)
# ============================================================================

test_that("maxret_opt handles factor exposure constraints via ROI", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  B <- matrix(c(1.2, 0.8, 1.0, 0.9, 1.1), ncol = 1)
  rownames(B) <- colnames(R5)
  colnames(B) <- "market"
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.9, upper = 1.1)
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  exposure <- sum(w * B[, 1])
  expect_true(exposure >= 0.9 - 1e-4)
  expect_true(exposure <= 1.1 + 1e-4)
})

# ============================================================================
# H. Max return with infinite box constraints (covers lines 194-199)
# ============================================================================

test_that("maxret_opt warns on infinite box constraints", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = -Inf, max = Inf)
  portf <- add.objective(portf, type = "return", name = "mean")
  
  expect_warning(
    optimize.portfolio(R5, portf, optimize_method = "ROI"),
    "Inf"
  )
})

# ============================================================================
# I. Min ETL with group constraints (covers lines 443-455 in etl_opt)
# ============================================================================

test_that("etl_opt handles group constraints via ROI", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4, 5)),
                          group_min = c(0.2, 0.3),
                          group_max = c(0.5, 0.7))
  portf <- add.objective(portf, type = "risk", name = "ES")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_true(sum(w[1:2]) >= 0.2 - 1e-4)
  expect_true(sum(w[3:5]) >= 0.3 - 1e-4)
})

# ============================================================================
# J. Min ETL with factor exposure constraints (covers lines 457-463)
# ============================================================================

test_that("etl_opt handles factor exposure constraints via ROI", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  B <- matrix(c(1.2, 0.8, 1.0, 0.9, 1.1), ncol = 1)
  rownames(B) <- colnames(R5)
  colnames(B) <- "market"
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.9, upper = 1.1)
  portf <- add.objective(portf, type = "risk", name = "ES")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  exposure <- sum(w * B[, 1])
  expect_true(exposure >= 0.9 - 1e-4)
  expect_true(exposure <= 1.1 + 1e-4)
})

# ============================================================================
# K. Max return MILP with position limit (covers maxret_milp_opt lines 290-398
#    and mean_etl_opt lines 1152-1162)
# ============================================================================

test_that("maxret_milp_opt works with position limit constraint", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.55)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  # At most 3 non-zero positions
  expect_true(sum(abs(w) > 1e-6) <= 3)
})

# ============================================================================
# L. MILP ETL with position limit (covers etl_milp_opt lines 514-658
#    and starr_obj_fun MILP branch lines 1184-1192)
# ============================================================================

test_that("etl_milp_opt works with position limit constraint", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.55)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.objective(portf, type = "risk", name = "ES")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_true(sum(abs(w) > 1e-6) <= 3)
})

# ============================================================================
# M. MILP max return with group constraints (covers lines 340-352)
# ============================================================================

test_that("maxret_milp_opt handles group constraints", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.55)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 4)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4, 5)),
                          group_min = c(0.2, 0.3),
                          group_max = c(0.5, 0.7))
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_true(sum(abs(w) > 1e-6) <= 4)
  expect_true(sum(w[1:2]) >= 0.2 - 1e-4)
})

# ============================================================================
# N. MILP ETL with group constraints (covers lines 585-597)
# ============================================================================

test_that("etl_milp_opt handles group constraints", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.55)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 4)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4, 5)),
                          group_min = c(0.2, 0.3),
                          group_max = c(0.5, 0.7))
  portf <- add.objective(portf, type = "risk", name = "ES")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_true(sum(abs(w) > 1e-6) <= 4)
})

# ============================================================================
# O. GMV with turnover constraint (covers gmv_opt_toc lines 675-819)
# ============================================================================

test_that("gmv_opt_toc works with turnover constraint via ROI", {
  skip_if_not_installed("corpcor")
  
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.constraint(portf, type = "turnover", turnover_target = 0.3)
  portf <- add.objective(portf, type = "risk", name = "var")
  
  init_weights <- rep(0.2, 5)
  names(init_weights) <- colnames(R5)
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_equal(length(w), 5)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# P. GMV with leverage constraint (covers gmv_opt_leverage lines 1000-1142)
# ============================================================================

test_that("gmv_opt_leverage works with leverage constraint via ROI", {
  skip_if_not_installed("corpcor")
  
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = -0.01, max_sum = 0.01)
  portf <- add.constraint(portf, type = "box", min = -0.5, max = 0.5)
  portf <- add.constraint(portf, type = "leverage_exposure",
                          leverage = 1.6)
  portf <- add.objective(portf, type = "risk", name = "var")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_equal(length(w), 5)
  # Leverage = sum of absolute weights
  expect_true(sum(abs(w)) <= 1.6 + 0.05)
})

# ============================================================================
# Q. Max Sharpe ratio via ROI (covers max_sr_opt + sharpe_obj_fun lines 1330-1377)
# ============================================================================

test_that("max_sr_opt finds maximum Sharpe ratio via ROI", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "var")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI", maxSR = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 5)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# R. Max STARR ratio via ROI (covers mean_etl_opt + starr_obj_fun lines 1147-1198)
# ============================================================================

test_that("mean_etl_opt finds maximum STARR ratio via ROI", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "ES")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI", maxSTARR = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 5)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# S. GMV with target return (covers lines 30-36 in gmv_opt)
# ============================================================================

test_that("gmv_opt handles target return constraint", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
  # Use a feasible target: mean of equal-weight portfolio
  target_ret <- mean(colMeans(R5))
  portf <- add.constraint(portf, type = "return", return_target = target_ret)
  portf <- add.objective(portf, type = "risk", name = "var")
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_equal(length(w), 5)
  port_ret <- sum(w * colMeans(R5))
  expect_equal(port_ret, target_ret, tolerance = 1e-4)
})

# ============================================================================
# T. MILP max return with factor exposure (covers lines 355-361)
# ============================================================================

test_that("maxret_milp_opt handles factor exposure constraints", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.55)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 4)
  B <- matrix(c(1.2, 0.8, 1.0, 0.9, 1.1), ncol = 1)
  rownames(B) <- colnames(R5)
  colnames(B) <- "market"
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.8, upper = 1.2)
  portf <- add.objective(portf, type = "return", name = "mean")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_true(sum(abs(w) > 1e-6) <= 4)
})

# ============================================================================
# U. MILP ETL with factor exposure (covers lines 599-606)
# ============================================================================

test_that("etl_milp_opt handles factor exposure constraints", {
  portf <- portfolio.spec(assets = colnames(R5))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.55)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 4)
  B <- matrix(c(1.2, 0.8, 1.0, 0.9, 1.1), ncol = 1)
  rownames(B) <- colnames(R5)
  colnames(B) <- "market"
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.8, upper = 1.2)
  portf <- add.objective(portf, type = "risk", name = "ES")
  
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  w <- extractWeights(opt)
  expect_true(sum(abs(w) > 1e-6) <= 4)
})
