##### test_extractrisk_coverage.R #####
# Coverage: extractrisk.R — alpha normalization + custom moment_setting

skip_if_not_installed("CVXR")

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]
w <- c(0.4, 0.3, 0.3)

# Baseline result with default alphas (0.05)
baseline <- extract_risk(R3, w)

# ===========================================================================
# 1. Alpha normalization: ES_alpha > 0.5 should be converted to 1 - alpha
# ===========================================================================

test_that("extract_risk normalizes ES_alpha > 0.5", {
  # ES_alpha = 0.95 should behave like ES_alpha = 0.05
  result <- extract_risk(R3, w, ES_alpha = 0.95)
  expect_equal(result$ES, baseline$ES, tolerance = 1e-4)
})

test_that("extract_risk normalizes CSM_alpha > 0.5", {
  result <- extract_risk(R3, w, CSM_alpha = 0.75)
  result_direct <- extract_risk(R3, w, CSM_alpha = 0.25)
  expect_equal(result$CSM, result_direct$CSM, tolerance = 1e-4)
})

test_that("extract_risk normalizes EQS_alpha > 0.5", {
  result <- extract_risk(R3, w, EQS_alpha = 0.51)
  result_direct <- extract_risk(R3, w, EQS_alpha = 0.49)
  expect_equal(result$EQS, result_direct$EQS, tolerance = 1e-4)
})

# ===========================================================================
# 2. Custom moment_setting with mu and sigma
# ===========================================================================

test_that("extract_risk uses custom moment_setting mu and sigma", {
  custom_mu <- colMeans(R3)
  custom_sigma <- cov(R3)
  result <- extract_risk(R3, w,
                          moment_setting = list(mu = custom_mu,
                                                sigma = custom_sigma))
  # Custom moments using sample mean/cov should match baseline
  expect_equal(as.numeric(result$mean), as.numeric(custom_mu %*% w), tolerance = 1e-8)
  expect_equal(as.numeric(result$StdDev),
               as.numeric(sqrt(t(w) %*% custom_sigma %*% w)),
               tolerance = 1e-8)
})
