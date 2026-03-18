##### test_optimize_coverage3.R #####
# Coverage: optimize.portfolio.R remaining uncovered paths

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]

portf <- portfolio.spec(assets = colnames(R3))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
portf <- add.objective(portf, type = "risk", name = "StdDev")

# ===========================================================================
# 1. Unknown optimize_method
# ===========================================================================

test_that("optimize.portfolio errors on unknown optimize_method", {
  expect_error(
    optimize.portfolio(R3, portf, optimize_method = "nonexistent_solver"),
    "Unknown optimize_method"
  )
})

# ===========================================================================
# 2. warm_start with wrong length
# ===========================================================================

test_that("optimize.portfolio warns on warm_start length mismatch", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  expect_warning(
    optimize.portfolio(R3, portf, optimize_method = "ROI",
                       warm_start = c(0.5, 0.5)),
    "length"
  )
})

# ===========================================================================
# 3. warm_start with mismatched asset names
# ===========================================================================

test_that("optimize.portfolio warns on warm_start name mismatch", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  bad_ws <- c(wrong1 = 0.33, wrong2 = 0.34, wrong3 = 0.33)
  expect_warning(
    optimize.portfolio(R3, portf, optimize_method = "ROI",
                       warm_start = bad_ws),
    "names"
  )
})


