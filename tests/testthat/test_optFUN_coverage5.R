##### test_optFUN_coverage5.R #####
# Coverage: optFUN.R — maxret_opt with target return constraint (lines 210-212),
# and factor exposure constraint path in gmv_opt (lines 83-89).

library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]

# ===========================================================================
# 1. maxret_opt — target return constraint
# ===========================================================================

test_that("maxret_opt with target return exercises the target constraint path", {
  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")

  cstr <- get_constraints(p)

  moments <- list(mean = colMeans(R4))

  # target != NA triggers lines 210-212 (target must be feasible)
  result <- maxret_opt(R = R4, constraints = cstr, moments = moments,
                       target = 0.008, solver = "glpk")
  expect_true(is.numeric(result$weights))
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 2. gmv_opt — factor exposure constraint
# ===========================================================================

test_that("gmv_opt with factor exposure exercises the B-matrix path", {
  p <- portfolio.spec(assets = colnames(R4))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "factor_exposure",
                      B = c(1.2, 0.8, 1.0, 0.9),
                      lower = 0.8, upper = 1.1)
  p <- add.objective(p, type = "risk", name = "var")

  cstr <- get_constraints(p)

  moments <- list(
    mean = colMeans(R4),
    var = cov(as.matrix(R4))
  )

  # lambda must be > 0 for quadprog (positive definite Q matrix)
  result <- gmv_opt(R = R4, constraints = cstr, moments = moments,
                    lambda = 1, target = NA,
                    lambda_hhi = 0, conc_groups = NULL)
  expect_true(is.numeric(result$weights))
  expect_equal(length(result$weights), 4)
  expect_true(all(!is.na(result$weights)))
})
