
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####

test_that("demo_max_Sharpe.R runs without error", {
  
  expect_error( suppressWarnings(source(system.file("demo/demo_max_Sharpe.R", package="PortfolioAnalytics"))), NA)
})

context("demo_max_Sharpe")

test_that("init.portf contains mean as an objective", 
          { expect_true(init.portf$objectives[[1]]$name == "mean") })

test_that("init.portf contains StdDev as an objective", 
          { expect_true(init.portf$objectives[[2]]$name == "StdDev") })

##### maxSR.lo.ROI #####
context("maxSR.lo.ROI")

test_that("maxSR.lo.ROI objective measure mean = 0.005845738", 
          { expect_equal(round(as.numeric(extractObjectiveMeasures(maxSR.lo.ROI)$mean), digits = 4) , round(0.005845738,digits = 4)  , tolerance=1e-6) })

test_that("maxSR.lo.ROI objective measure StdDev = 0.01262847", 
          { expect_equal(round(as.numeric(extractObjectiveMeasures(maxSR.lo.ROI)$StdDev),digits = 4), round(0.01262847, digits = 4) , tolerance=1e-6) })

##### maxSR.lo.RP #####
context("maxSR.lo.RP")

test_that("maxSR.lo.RP objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxSR.lo.RP)$mean)) })

test_that("maxSR.lo.RP objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxSR.lo.RP)$StdDev)) })

##### maxSR.lo.DE #####
context("maxSR.lo.DE")

test_that("maxSR.lo.DE objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxSR.lo.DE)$mean)) })

test_that("maxSR.lo.DE objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxSR.lo.DE)$StdDev)) })

