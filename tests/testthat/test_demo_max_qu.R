##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

context("test demo_max_quadratic_utility.R")

##### Source Demo Script #####

test_that("demo_max_quadratic_utility.R runs succesfully", {
  expect_error(source(system.file("demo/demo_max_quadratic_utility.R", package="PortfolioAnalytics")), NA)
})

##### init.portf objectives #####
context("objectives for quadratic utility")

test_that("init.portf contains mean as an objective", 
          { expect_true(init.portf$objectives[[1]]$name == "mean") })

test_that("init.portf contains StdDev as an objective", 
          { expect_true(init.portf$objectives[[2]]$name == "StdDev") })

##### ROI, full_investment, long only, max qu ######
context("maxQU.lo.ROI")

test_that("risk aversion parameter = 4", 
          { expect_equal(maxQU.lo.ROI$portfolio$objectives[[2]]$risk_aversion, 4) })

test_that("maxQU.lo.ROI objective measure mean = 0.006484776", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.lo.ROI)$mean), 0.006484776, tolerance=1e-6) })

test_that("maxQU.lo.ROI objective measure StdDev = 0.01578019", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.lo.ROI)$StdDev), 0.01578019, tolerance=1e-6) })

test_that("maxQU.lo.ROI min box constraints are not violated", 
          { expect_true(all(extractWeights(maxQU.lo.ROI) + 0.00000001 >= maxQU.lo.ROI$portfolio$constraints[[2]]$min)) })

test_that("maxret.lo.ROI max box constraints are not violated", 
          { expect_true(all(extractWeights(maxQU.lo.ROI) <= maxQU.lo.ROI$portfolio$constraints[[2]]$max)) })

##### ROI, full_investment, long only, max qu to approximate max return ######
context("maxQU.maxret.ROI")

test_that("risk aversion parameter = 1e-5", 
          { expect_equal(maxQU.maxret.ROI$portfolio$objectives[[2]]$risk_aversion, 1e-5) })

test_that("maxQU.maxret.ROI objective measure mean = 0.006621818", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.maxret.ROI)$mean), 0.006621818, tolerance=1e-6) })

test_that("maxQU.maxret.ROI objective measure StdDev = 0.01688505", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.maxret.ROI)$StdDev), 0.01688505, tolerance=1e-6) })

test_that("maxQU.maxret.ROI min box constraints are not violated", 
          { expect_true(all(extractWeights(maxQU.maxret.ROI) + 0.00000001 >= maxQU.maxret.ROI$portfolio$constraints[[2]]$min)) })

test_that("maxQU.maxret.ROI max box constraints are not violated", 
          { expect_true(all(extractWeights(maxQU.maxret.ROI) <= maxQU.maxret.ROI$portfolio$constraints[[2]]$max)) })

##### ROI, full_investment, long only, max qu to approximate min StdDev ######
context("maxQU.minvol.ROI")

test_that("risk aversion parameter = 1e6", 
          { expect_equal(maxQU.minvol.ROI$portfolio$objectives[[2]]$risk_aversion, 1e6) })

test_that("maxQU.minvol.ROI objective measure mean = 0.004581024", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.minvol.ROI)$mean), 0.004581024, tolerance=1e-6) })

test_that("maxQU.minvol.ROI objective measure StdDev = 0.007188931", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxQU.minvol.ROI)$StdDev), 0.007188931, tolerance=1e-6) })

test_that("maxQU.minvol.ROI min box constraints are not violated", 
          { expect_true(all(extractWeights(maxQU.minvol.ROI) + 0.00000001 >= maxQU.minvol.ROI$portfolio$constraints[[2]]$min)) })

test_that("maxQU.minvol.ROI max box constraints are not violated", 
          { expect_true(all(extractWeights(maxQU.minvol.ROI) <= maxQU.minvol.ROI$portfolio$constraints[[2]]$max)) })
