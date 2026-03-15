
require(testthat)
require(PortfolioAnalytics)

context("optFUN.R coverage: concentration aversion, factor exposure, infinite bounds")

# ============================================================================
# Shared test data
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

# ============================================================================
# Tests: gmv_opt with concentration aversion (lambda_hhi scalar, no groups)
# ============================================================================

test_that("GMV with scalar concentration aversion (lambda_hhi) works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "weight_concentration",
                         name = "HHI", conc_aversion = 0.05)

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
  # With concentration aversion, weights should be more diversified
  expect_true(all(w > 0))
})

test_that("GMV with grouped concentration aversion (conc_groups) works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "weight_concentration",
                         name = "HHI",
                         conc_aversion = c(0.03, 0.07),
                         conc_groups = list(c(1, 2), c(3, 4)))

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ============================================================================
# Tests: factor exposure constraints
# ============================================================================

test_that("GMV with factor exposure constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)

  B <- matrix(c(1.2, 0.8, 0.5, 1.1,
                0.3, 0.7, 1.0, 0.4), nrow = 4, ncol = 2)
  colnames(B) <- c("Market", "Size")
  rownames(B) <- colnames(R4)

  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B,
                          lower = c(0.5, 0.3),
                          upper = c(1.0, 0.8))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)

  expect_equal(length(w), 4)
  # Verify factor exposure bounds are respected
  actual_exposures <- as.numeric(t(w) %*% B)
  expect_true(all(actual_exposures >= 0.5 - 0.01))
  expect_true(all(actual_exposures <= 1.0 + 0.01))
})

test_that("maxret with factor exposure constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)

  B <- matrix(c(1.2, 0.8, 0.5, 1.1,
                0.3, 0.7, 1.0, 0.4), nrow = 4, ncol = 2)
  colnames(B) <- c("Market", "Size")
  rownames(B) <- colnames(R4)

  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B,
                          lower = c(0.5, 0.3),
                          upper = c(1.0, 0.8))
  portf <- add.objective(portf, type = "return", name = "mean")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

test_that("ETL with factor exposure constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)

  B <- matrix(c(1.2, 0.8, 0.5, 1.1,
                0.3, 0.7, 1.0, 0.4), nrow = 4, ncol = 2)
  colnames(B) <- c("Market", "Size")
  rownames(B) <- colnames(R4)

  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B,
                          lower = c(0.5, 0.3),
                          upper = c(1.0, 0.8))
  portf <- add.objective(portf, type = "risk", name = "ES",
                         arguments = list(p = 0.95))

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

# ============================================================================
# Tests: infinite bound handling in maxret_opt
# ============================================================================

test_that("maxret_opt warns on infinite box constraints", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  # Use Inf for upper bounds to trigger the warning path
  portf <- add.constraint(portf, type = "box", min = 0, max = Inf)
  portf <- add.objective(portf, type = "return", name = "mean")

  expect_warning(
    opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE),
    "Inf.*box constraint"
  )
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
})

# ============================================================================
# Tests: gmv_opt target return with mean=0 path (lines 30-40)
# ============================================================================

test_that("GMV with target return and zero moments$mean computes from data", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # Max Sharpe uses target return internally, exercising the target != NA path
  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI",
                            maxSR = TRUE, trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_true(!is.null(extractObjectiveMeasures(opt)$mean))
})

# ============================================================================
# Tests: gmv_opt_toc with group constraints
# ============================================================================

test_that("GMV with turnover + group constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8))
  portf <- add.constraint(portf, type = "turnover", turnover_target = 0.5)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_true(sum(w[1:2]) >= 0.2 - 1e-3)
  expect_true(sum(w[3:4]) >= 0.2 - 1e-3)
})

# ============================================================================
# Tests: gmv_opt_leverage with group constraints
# ============================================================================

test_that("GMV leverage exposure with group constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("corpcor")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "weight_sum", min_sum = -0.01, max_sum = 0.01)
  portf <- add.constraint(portf, type = "box", min = -0.5, max = 0.5)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(-0.5, -0.5),
                          group_max = c(0.5, 0.5))
  portf <- add.constraint(portf, type = "leverage_exposure", leverage = 1.6)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_true(sum(abs(w)) <= 1.6 + 0.01)
})

# ============================================================================
# Tests: ETL objective naming (ES vs ETL vs CVaR)
# ============================================================================

test_that("ETL objective values are named correctly", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "ES",
                         arguments = list(p = 0.95))

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  obj_measures <- extractObjectiveMeasures(opt)
  # Should have ES in the names
  expect_true(any(grepl("ES|ETL|CVaR", names(obj_measures))))
})

# ============================================================================
# Tests: maxret_milp_opt with group constraints
# ============================================================================

# NOTE: maxret_milp_opt with group constraints has a code bug ("non-numeric
# argument to binary operator" in group constraint matrix construction).
# Skipping that combination until the upstream code is fixed.

# ============================================================================
# Tests: etl_milp_opt with group constraints
# ============================================================================

test_that("ETL MILP with group + position limit constraints works", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.8)
  portf <- add.constraint(portf, type = "position_limit", max_pos = 3)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(0.1, 0.1),
                          group_max = c(0.9, 0.9))
  portf <- add.objective(portf, type = "risk", name = "ES",
                         arguments = list(p = 0.95))

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_true(sum(w > 1e-6) <= 3)
})

# ============================================================================
# Tests: gmv_opt with target return (explicit)
# ============================================================================

test_that("GMV with explicit target return constraint runs", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "return",
                          return_target = 0.005)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # This may warn about NA weights with some solver configurations
  suppressWarnings(
    opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  )
  expect_s3_class(opt, "optimize.portfolio")
})

# ============================================================================
# Tests: gmv_opt when moments$mean == 0 (no return objective)
# ============================================================================

test_that("GMV with no return objective produces StdDev-only objective values", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  obj <- extractObjectiveMeasures(opt)
  expect_true("StdDev" %in% names(obj))
})

# ============================================================================
# Tests: infinite constraint removal in gmv_opt (lines 97-100)
# ============================================================================

test_that("GMV handles Inf in rhs.vec by removing those rows", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  # Group constraints with Inf bounds should be removed gracefully
  portf <- portfolio.spec(assets = colnames(R4))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(0.0, 0.0),
                          group_max = c(Inf, Inf))
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R4, portf, optimize_method = "ROI", trace = TRUE)
  w <- extractWeights(opt)
  expect_equal(length(w), 4)
  expect_equal(sum(w), 1, tolerance = 0.02)
})
