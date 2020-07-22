
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####

context("test demo_weight_concentration.R")

test_that("demo_weight_concentration.R runs succesfully", {
  expect_error(source(system.file("demo/demo_weight_concentration.R", package="PortfolioAnalytics")), NA)
})


context("weight concentration objective")

test_that("conc.portf contains weight_concentration as an objective", 
          { expect_true(inherits(conc.portf$objectives[[2]], "weight_concentration_objective")) })

test_that("conc.portf contains weight_concentration as an objective", 
          { expect_true(conc.portf$objectives[[2]]$name == "HHI") })

test_that("conc.portf contains weight_concentration as an objective with conc_aversion=0", 
          { expect_equal(opt2$portfolio$objectives[[2]]$conc_aversion, rep(0, 4)) })

test_that("minimum variance and conc.portf weights are equal with conc_aversion=0", 
          { expect_equal(opt1$weights, opt2$weights) })

test_that("conc.portf with conc_aversion=1e6 results in an equal weight portfolio", 
          { expect_equal(as.numeric(opt4$weights), rep(1 / 8, 8)) })

