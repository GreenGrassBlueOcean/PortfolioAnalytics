
require(testthat)
require(PortfolioAnalytics)

context("objectives")

N <- 4
init.portf <- portfolio.spec(assets=N)
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean", target=0.005)
init.portf <- add.objective(portfolio=init.portf, type="risk", name="ES", arguments=list(p=0.95))
init.portf <- add.objective(portfolio=init.portf, type="risk_budget", name="ES")

test_that("return objective is consistent", {
  expect_equal(init.portf$objectives[[1]]$name, "mean")
  expect_equal(init.portf$objectives[[1]]$target, 0.005)
  expect_true(init.portf$objectives[[1]]$enabled)
  expect_equal(init.portf$objectives[[1]]$multiplier, -1)
  expect_equal(class(init.portf$objectives[[1]]), c("return_objective", "objective"))
})

test_that("risk objective is consistent", {
  expect_equal(init.portf$objectives[[2]]$name, "ES")
  expect_true(is.null(init.portf$objectives[[2]]$target))
  expect_equal(init.portf$objectives[[2]]$arguments$portfolio_method, "single")
  expect_equal(init.portf$objectives[[2]]$arguments$p, 0.95)
  expect_true(init.portf$objectives[[2]]$enabled)
  expect_equal(init.portf$objectives[[2]]$multiplier, 1)
  expect_equal(class(init.portf$objectives[[2]]), c("portfolio_risk_objective", "objective"))
})

test_that("risk objective is consistent", {
  expect_equal(init.portf$objectives[[3]]$name, "ES")
  expect_true(is.null(init.portf$objectives[[3]]$target))
  expect_equal(init.portf$objectives[[3]]$arguments$portfolio_method, "component")
  expect_true(init.portf$objectives[[3]]$enabled)
  expect_equal(init.portf$objectives[[3]]$multiplier,1)
  expect_true(init.portf$objectives[[3]]$min_concentration)
  expect_false(init.portf$objectives[[3]]$min_difference)
  expect_equal(class(init.portf$objectives[[3]]), c("risk_budget_objective", "objective"))
})
