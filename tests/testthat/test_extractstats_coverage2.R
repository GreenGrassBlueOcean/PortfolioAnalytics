##### test_extractstats_coverage2.R #####
# Phase 3B coverage: extractStats for invol/eqwt, name.replace aliases,
#   opt.list/opt.rebal.list methods


data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# ===========================================================================
# A. name.replace — uncommon alias branches
# ===========================================================================

test_that("name.replace handles median.median", {
  out <- name.replace(c("median.median", "foo"))
  expect_equal(out[1], "median")
})

test_that("name.replace handles CVaR.MES", {
  out <- name.replace(c("CVaR.MES"))
  expect_equal(out[1], "CVaR")
})

test_that("name.replace handles ETL.MES", {
  out <- name.replace(c("ETL.MES"))
  expect_equal(out[1], "ETL")
})

test_that("name.replace handles ETL.ETL", {
  out <- name.replace(c("ETL.ETL"))
  expect_equal(out[1], "ETL")
})

test_that("name.replace handles VaR.MVaR", {
  out <- name.replace(c("VaR.MVaR"))
  expect_equal(out[1], "VaR")
})

test_that("name.replace handles maxDrawdown.maxDrawdown", {
  out <- name.replace(c("maxDrawdown.maxDrawdown"))
  expect_equal(out[1], "maxDrawdown")
})

# ===========================================================================
# B. extractStats for equal.weight and inverse.volatility
# ===========================================================================

portf_simple <- portfolio.spec(assets = colnames(R5))
portf_simple <- add.constraint(portf_simple, type = "full_investment")
portf_simple <- add.constraint(portf_simple, type = "long_only")
portf_simple <- add.objective(portf_simple, type = "risk", name = "StdDev")

test_that("extractStats works for equal.weight result", {
  opt_eqwt <- equal.weight(R5, portf_simple)
  stats <- extractStats(opt_eqwt)
  expect_true(is.numeric(stats))
  expect_true("out" %in% names(stats))
  expect_true(any(grepl("^w\\.", names(stats)) | grepl("^A$|^B$|^C$|^D$|^E$", names(stats))))
})

test_that("extractStats works for inverse.volatility result", {
  opt_invol <- inverse.volatility.weight(R5, portf_simple)
  stats <- extractStats(opt_invol)
  expect_true(is.numeric(stats))
  expect_true("out" %in% names(stats))
})

# ===========================================================================
# C. extractStats.opt.list
# ===========================================================================

test_that("extractStats works for opt.list", {
  opt1 <- optimize.portfolio(R5, portf_simple, optimize_method = "ROI")
  opt2 <- optimize.portfolio(R5, portf_simple, optimize_method = "ROI")
  opt_list <- combine.optimizations(list(opt1 = opt1, opt2 = opt2))
  stats_list <- extractStats(opt_list)
  expect_true(is.list(stats_list))
  expect_equal(length(stats_list), 2)
})

# ===========================================================================
# D. extractStats.opt.rebal.list
# ===========================================================================

test_that("extractStats works for opt.rebal.list", {
  portf1 <- portfolio.spec(assets = colnames(R5))
  portf1 <- add.constraint(portf1, type = "full_investment")
  portf1 <- add.constraint(portf1, type = "long_only")
  portf1 <- add.objective(portf1, type = "risk", name = "StdDev")

  portf2 <- portfolio.spec(assets = colnames(R5))
  portf2 <- add.constraint(portf2, type = "full_investment")
  portf2 <- add.constraint(portf2, type = "long_only")
  portf2 <- add.objective(portf2, type = "risk", name = "ES")

  rebal1 <- optimize.portfolio.rebalancing(
    R5, portf1, optimize_method = "ROI",
    rebalance_on = "quarters", training_period = 24
  )
  rebal2 <- optimize.portfolio.rebalancing(
    R5, portf2, optimize_method = "ROI",
    rebalance_on = "quarters", training_period = 24
  )

  orl <- structure(list(rebal1, rebal2), class = "opt.rebal.list")
  stats <- extractStats(orl)
  expect_true(is.list(stats))
  expect_equal(length(stats), 2)
})
