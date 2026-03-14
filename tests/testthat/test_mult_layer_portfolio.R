library(testthat)
library(PortfolioAnalytics)

context("Multi-layer portfolio: sub.portfolio internals and proxy validation")

data(edhec)
R6 <- edhec[1:60, 1:6]
colnames(R6) <- paste0("A", 1:6)

# ============================================================================
# A. sub.portfolio internal constructor
# ============================================================================

test_that("sub.portfolio creates correct structure", {
  portf <- portfolio.spec(assets = colnames(R6)[1:3])
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  sp <- PortfolioAnalytics:::sub.portfolio(
    portfolio = portf,
    optimize_method = "random",
    search_size = 100,
    rebalance_on = "quarters",
    training_period = 36
  )

  expect_s3_class(sp, "sub.portfolio")
  expect_true(is.portfolio(sp$portfolio))
  expect_equal(sp$optimize_method, "random")
  expect_equal(sp$search_size, 100)
  expect_equal(sp$rebalance_on, "quarters")
  expect_equal(sp$training_period, 36)
  expect_null(sp$trailing_periods)
  expect_null(sp$rp)
})

test_that("sub.portfolio rejects non-portfolio object", {
  expect_error(
    PortfolioAnalytics:::sub.portfolio(portfolio = list(), optimize_method = "random"),
    "not of class"
  )
})

test_that("sub.portfolio passes through extra arguments", {
  portf <- portfolio.spec(assets = colnames(R6)[1:3])
  portf <- add.constraint(portf, type = "full_investment")

  sp <- PortfolioAnalytics:::sub.portfolio(
    portfolio = portf,
    optimize_method = "DEoptim",
    custom_param = "test_value"
  )
  expect_equal(sp$custom_param, "test_value")
})

# ============================================================================
# B. add.sub.portfolio parameter forwarding
# ============================================================================

test_that("add.sub.portfolio forwards all parameters to sub.portfolio", {
  top <- portfolio.spec(assets = paste0("proxy", 1:2))
  top <- add.constraint(top, type = "full_investment")

  sub1 <- portfolio.spec(assets = colnames(R6)[1:3])
  sub1 <- add.constraint(sub1, type = "full_investment")
  sub1 <- add.objective(sub1, type = "risk", name = "StdDev")

  mp <- mult.portfolio.spec(top)
  mp <- add.sub.portfolio(mp, sub1,
                          optimize_method = "random",
                          search_size = 500,
                          rebalance_on = "months",
                          training_period = 24,
                          trailing_periods = 12)

  sp <- mp$sub.portfolios[[1]]
  expect_equal(sp$search_size, 500)
  expect_equal(sp$rebalance_on, "months")
  expect_equal(sp$training_period, 24)
  expect_equal(sp$trailing_periods, 12)
})

# ============================================================================
# C. proxy.mult.portfolio input validation
# ============================================================================

test_that("proxy.mult.portfolio rejects non mult.portfolio.spec", {
  expect_error(
    PortfolioAnalytics:::proxy.mult.portfolio(R6, list()),
    "mult.portfolio.spec"
  )
})

test_that("proxy.mult.portfolio requires more than 1 sub portfolio", {
  top <- portfolio.spec(assets = "proxy1")
  mp <- mult.portfolio.spec(top)

  sub1 <- portfolio.spec(assets = colnames(R6)[1:3])
  sub1 <- add.constraint(sub1, type = "full_investment")
  sub1 <- add.objective(sub1, type = "risk", name = "StdDev")
  mp <- add.sub.portfolio(mp, sub1, optimize_method = "random")

  expect_error(
    PortfolioAnalytics:::proxy.mult.portfolio(R6, mp),
    "more than 1"
  )
})

# ============================================================================
# D. mult.portfolio.spec with extra arguments
# ============================================================================

test_that("mult.portfolio.spec passes through extra arguments", {
  portf <- portfolio.spec(assets = c("p1", "p2"))
  mp <- mult.portfolio.spec(portf, levels = 3, custom_field = "hello")
  expect_s3_class(mp, "mult.portfolio.spec")
  expect_equal(mp$custom_field, "hello")
})
