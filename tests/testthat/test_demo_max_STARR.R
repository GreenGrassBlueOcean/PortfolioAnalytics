
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)


context("test demo max STARR")

##### Source Demo Script #####
test_that("demo_max_STARR.R runs succesfully", {
expect_error(source(system.file("demo/demo_max_STARR.R", package="PortfolioAnalytics")), NA)
})

test_that("init.portf contains mean as an objective", 
          { expect_true(init.portf$objectives[[1]]$name == "mean") })

test_that("init.portf contains ES as an objective", 
          { expect_true(init.portf$objectives[[2]]$name == "ES") })

test_that("init.portf contains ES as an objective with p=0.925", 
          { expect_equal(init.portf$objectives[[2]]$arguments$p, 0.925) })

##### maxSR.lo.ROI #####
context("maxSTARR.lo.ROI")

test_that("maxSTARR.lo.ROI objective measure mean = 0.004803075", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxSTARR.lo.ROI)$mean), 0.004803075, tolerance=1e-6) })

test_that("maxSTARR.lo.ROI objective measure ES = 0.01304417", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxSTARR.lo.ROI)$ES), 0.01304417, tolerance=1e-6) })

##### maxSTARR.lo.RP #####
context("maxSTARR.lo.RP")

test_that("maxSTARR.lo.RP objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxSTARR.lo.RP)$mean)) })

test_that("maxSTARR.lo.RP objective measure ES is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxSTARR.lo.RP)$ES)) })

##### maxSTARR.lo.DE #####
context("maxSTARR.lo.DE")

test_that("maxSTARR.lo.DE objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxSTARR.lo.DE)$mean)) })

test_that("maxSR.lo.DE objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxSTARR.lo.DE)$ES)) })
