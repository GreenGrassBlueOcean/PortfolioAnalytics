
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####

context("test demo_risk_budgets.R")

test_that("demo_risk_budgets.R runs succesfully", {
  expect_error(source(system.file("demo/demo_risk_budgets.R", package="PortfolioAnalytics")), NA)
})

context("risk budget objective ES max_prisk")

test_that("rbES.portf contains risk_budget_objective", 
          { expect_true(inherits(rbES.portf$objectives[[2]], "risk_budget_objective")) })

test_that("rbES.portf contains ES risk budget objective", 
          { expect_true(rbES.portf$objectives[[2]]$name == "ES") })

test_that("rbES.portf max_prisk is 0.4", 
          { expect_equal(as.numeric(rbES.portf$objectives[[2]]$max_prisk), rep(0.4, 8)) })

test_that("rbES.portf min_concentration is false", 
          { expect_false(rbES.portf$objectives[[2]]$min_concentration) })

test_that("rbES.portf min_difference is false", 
          { expect_false(rbES.portf$objectives[[2]]$min_difference) })

test_that("rbES.DE optimal weights is a numeric vector", 
          { expect_true(is.numeric(extractWeights(rbES.DE))) })

test_that("rbES.DE optimization does not violate max_prisk", 
          { expect_true(all(extractObjectiveMeasures(rbES.DE)$ES$pct_contrib_MES <= 0.4)) })

context("risk budget objective ES min_concentration")

test_that("eqES.portf contains risk_budget_objective", 
          { expect_true(inherits(eqES.portf$objectives[[2]], "risk_budget_objective")) })

test_that("eqES.portf contains ES risk budget objective", 
          { expect_true(eqES.portf$objectives[[2]]$name == "ES") })

test_that("eqES.portf min_concentration is false", 
          { expect_true(eqES.portf$objectives[[2]]$min_concentration) })

test_that("eqES.portf min_difference is false", 
          { expect_false(eqES.portf$objectives[[2]]$min_difference) })

test_that("eqES.RP optimal weights is a numeric vector", 
          { expect_true(is.numeric(extractWeights(eqES.RP))) })

# This will be difficult to test for an exact value or limit
test_that("eqES.RP optimization pct_contrib_MES is a numeric vector", 
          { expect_true(is.numeric(extractObjectiveMeasures(eqES.RP)$ES$pct_contrib_MES)) })

context("risk budget objective StdDev max_prisk")

test_that("rbStdDev.portf contains risk_budget_objective", 
          { expect_true(inherits(rbStdDev.portf$objectives[[2]], "risk_budget_objective")) })

test_that("rbStdDev.portf contains StdDev risk budget objective", 
          { expect_true(rbStdDev.portf$objectives[[2]]$name == "StdDev") })

test_that("rbStdDev.portf max_prisk is 0.25", 
          { expect_equal(as.numeric(rbStdDev.portf$objectives[[2]]$max_prisk), rep(0.25, 8)) })

test_that("rbStdDev.portf min_concentration is false", 
          { expect_false(rbStdDev.portf$objectives[[2]]$min_concentration) })

test_that("rbStdDev.portf min_difference is false", 
          { expect_false(rbStdDev.portf$objectives[[2]]$min_difference) })

test_that("rbStdDev.DE optimal weights is a numeric vector", 
          { expect_true(is.numeric(extractWeights(rbStdDev.DE))) })

test_that("rbStdDev.DE optimization does not violate max_prisk", 
          { expect_true(all(extractObjectiveMeasures(rbStdDev.DE)$ES$pct_contrib_MES <= 0.25)) })

