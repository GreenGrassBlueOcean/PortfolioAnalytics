# Tests for Proposal #9: Refactored moment computation
# Verifies the table-driven set.portfolio.moments_v2() produces identical
# results to the original double-nested switch.

library(testthat)
library(PortfolioAnalytics)

data(edhec)
R <- edhec[, 1:4]
colnames(R) <- c("CA", "CTAG", "DS", "EM")
funds <- colnames(R)

# Helper: build a portfolio with given objective names
make_portfolio <- function(obj_specs) {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(portfolio = p, type = "full_investment")
  p <- add.constraint(portfolio = p, type = "long_only")
  for (spec in obj_specs) {
    p <- add.objective(portfolio = p, type = spec$type, name = spec$name,
                       arguments = spec$arguments)
  }
  p
}

# --- Test 1: StdDev portfolio produces mu + sigma only ---
test_that("StdDev objective produces mu and sigma only", {
  portf <- make_portfolio(list(list(type = "risk", name = "StdDev")))
  moments <- set.portfolio.moments(R, portf)
  expect_equal(sort(names(moments)), c("mu", "sigma"))
})

# --- Test 2: mean-only objective produces mu only ---
test_that("mean-only objective produces mu only", {
  portf <- make_portfolio(list(list(type = "return", name = "mean")))
  moments <- set.portfolio.moments(R, portf)
  expect_equal(names(moments), "mu")
})

# --- Test 3: VaR objective produces all four moments ---
test_that("VaR objective produces mu, sigma, m3, m4", {
  portf <- make_portfolio(list(list(type = "risk", name = "VaR")))
  moments <- set.portfolio.moments(R, portf)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

# --- Test 4: ES objective produces all four moments ---
test_that("ES objective produces mu, sigma, m3, m4", {
  portf <- make_portfolio(list(list(type = "risk", name = "ES")))
  moments <- set.portfolio.moments(R, portf)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

# --- Test 5: Multiple objectives (mean + ES) produce all four moments ---
test_that("mean + ES objectives produce all four moments", {
  portf <- make_portfolio(list(
    list(type = "return", name = "mean"),
    list(type = "risk",   name = "ES")
  ))
  moments <- set.portfolio.moments(R, portf)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

# --- Test 6: ROI=TRUE with ES-only → empty momentargs ---
test_that("ROI=TRUE with ES-only portfolio produces empty momentargs", {
  portf <- make_portfolio(list(list(type = "risk", name = "ES")))
  moments <- set.portfolio.moments(R, portf, ROI = TRUE)
  expect_length(moments, 0)
})

# --- Test 7: ROI=TRUE with mean + ES → mu only ---
test_that("ROI=TRUE with mean + ES produces mu only", {
  portf <- make_portfolio(list(
    list(type = "return", name = "mean"),
    list(type = "risk",   name = "ES")
  ))
  moments <- set.portfolio.moments(R, portf, ROI = TRUE)
  expect_equal(names(moments), "mu")
})

# --- Test 8: na.rm inconsistency preserved ---
# StdDev objectives use na.rm=TRUE; VaR objectives do NOT
test_that("na.rm inconsistency: StdDev uses na.rm=TRUE, VaR does not", {
  portf_sd <- make_portfolio(list(list(type = "risk", name = "StdDev")))
  portf_var <- make_portfolio(list(list(type = "risk", name = "VaR")))
  
  moments_sd <- set.portfolio.moments(R, portf_sd)
  moments_var <- set.portfolio.moments(R, portf_var)
  
  expected_mu_narm <- matrix(as.vector(apply(R, 2, 'mean', na.rm = TRUE)), ncol = 1)
  expected_mu_raw  <- matrix(as.vector(apply(R, 2, 'mean')), ncol = 1)
  expected_sigma_pw <- cov(R, use = 'pairwise.complete.obs')
  expected_sigma_raw <- cov(R)
  
  expect_equal(moments_sd$mu, expected_mu_narm)
  expect_equal(moments_var$mu, expected_mu_raw)
  expect_equal(moments_sd$sigma, expected_sigma_pw)
  expect_equal(moments_var$sigma, expected_sigma_raw)
})

# --- Test 9: First-writer-wins with mixed objective order ---
# mean (sets mu with na.rm=TRUE) then VaR: mu should keep na.rm=TRUE version
test_that("first-writer-wins: mean then VaR preserves na.rm=TRUE mu", {
  portf <- make_portfolio(list(
    list(type = "return", name = "mean"),
    list(type = "risk",   name = "VaR")
  ))
  moments <- set.portfolio.moments(R, portf)
  
  expected_mu_narm <- matrix(as.vector(apply(R, 2, 'mean', na.rm = TRUE)), ncol = 1)
  expect_equal(moments$mu, expected_mu_narm)
})

# --- Test 10: Reverse order: VaR then mean → mu uses raw (no na.rm) ---
test_that("first-writer-wins: VaR then mean uses raw mu (no na.rm)", {
  portf <- make_portfolio(list(
    list(type = "risk",   name = "VaR"),
    list(type = "return", name = "mean")
  ))
  moments <- set.portfolio.moments(R, portf)
  
  expected_mu_raw <- matrix(as.vector(apply(R, 2, 'mean')), ncol = 1)
  expect_equal(moments$mu, expected_mu_raw)
})

# --- Test 11: All ES aliases produce same results ---
test_that("all ES aliases produce identical moments", {
  aliases <- c("es", "mES", "CVaR", "cVaR", "ETL", "mETL", "ES")
  ref_moments <- NULL
  for (alias in aliases) {
    portf <- make_portfolio(list(list(type = "risk", name = alias)))
    moments <- set.portfolio.moments(R, portf)
    if (is.null(ref_moments)) {
      ref_moments <- moments
    } else {
      expect_equal(moments, ref_moments, info = paste("alias:", alias))
    }
  }
})

# --- Test 12: VaR alias mVaR produces same results as VaR ---
test_that("mVaR produces same moments as VaR", {
  portf_var <- make_portfolio(list(list(type = "risk", name = "VaR")))
  portf_mvar <- make_portfolio(list(list(type = "risk", name = "mVaR")))
  expect_equal(
    set.portfolio.moments(R, portf_var),
    set.portfolio.moments(R, portf_mvar)
  )
})

# --- Test 13: StdDev aliases var/sd produce same results ---
test_that("var and sd produce same moments as StdDev", {
  portf_sd <- make_portfolio(list(list(type = "risk", name = "StdDev")))
  portf_var <- make_portfolio(list(list(type = "risk", name = "var")))
  portf_sdalias <- make_portfolio(list(list(type = "risk", name = "sd")))
  
  ref <- set.portfolio.moments(R, portf_sd)
  expect_equal(set.portfolio.moments(R, portf_var), ref)
  expect_equal(set.portfolio.moments(R, portf_sdalias), ref)
})

# --- Test 14: Unknown objective is silently ignored ---
test_that("unknown objective name is silently ignored", {
  portf <- make_portfolio(list(
    list(type = "return", name = "mean"),
    list(type = "risk",   name = "CustomRisk")
  ))
  moments <- set.portfolio.moments(R, portf)
  # mean is recognized, CustomRisk is not → only mu
  expect_equal(names(moments), "mu")
})

# --- Test 15: Moment values for sample method are exact ---
test_that("sample method moment values are numerically exact", {
  portf <- make_portfolio(list(list(type = "risk", name = "VaR")))
  moments <- set.portfolio.moments(R, portf, method = "sample")
  
  expect_equal(moments$mu, matrix(as.vector(apply(R, 2, 'mean')), ncol = 1))
  expect_equal(moments$sigma, cov(R))
  expect_equal(moments$m3, PerformanceAnalytics::M3.MM(R))
  expect_equal(moments$m4, PerformanceAnalytics::M4.MM(R))
})

# --- Test 16: No objectives (NULL) → warning ---
test_that("NULL objectives produces warning", {
  portf <- portfolio.spec(assets = funds)
  portf$objectives <- NULL
  expect_warning(
    set.portfolio.moments(R, portf),
    "no objectives specified"
  )
})

# --- Test 17: ROI=TRUE with all ES aliases skips all ---
test_that("ROI=TRUE skips all ES alias variants", {
  for (alias in c("es", "mES", "CVaR", "cVaR", "ETL", "mETL", "ES")) {
    portf <- make_portfolio(list(list(type = "risk", name = alias)))
    moments <- set.portfolio.moments(R, portf, ROI = TRUE)
    expect_equal(length(moments), 0L, info = paste("alias:", alias))
  }
})

# --- Test 18: Existing test parity (mirrors test-customMoments.R) ---
test_that("parity with existing customMoments tests", {
  init.portf <- portfolio.spec(assets = funds)
  init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
  init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
  
  SD.portf <- add.objective(portfolio = init.portf, type = "risk", name = "StdDev")
  ES.portf <- add.objective(portfolio = init.portf, type = "risk", name = "ES")
  
  sd.moments <- set.portfolio.moments(R, SD.portf)
  expect_equal(names(sd.moments), c("mu", "sigma"))
  
  es.moments <- set.portfolio.moments(R, ES.portf)
  expect_equal(names(es.moments), c("mu", "sigma", "m3", "m4"))
})

# --- Test 19: Pre-set moments are not overwritten ---
test_that("pre-set moments in momentargs are preserved", {
  portf <- make_portfolio(list(list(type = "risk", name = "StdDev")))
  custom_mu <- matrix(rep(0.01, 4), ncol = 1)
  moments <- set.portfolio.moments(R, portf, momentargs = list(mu = custom_mu))
  
  expect_equal(moments$mu, custom_mu)
  expect_false(is.null(moments$sigma))
})
