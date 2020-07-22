##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

context("test demo_max_return.R")

##### Source Demo Script #####

test_that("demo_max_return.R runs succesfully", {
  expect_error(source(system.file("demo/demo_max_return.R", package="PortfolioAnalytics")), NA)
})


###### ROI, full_investment, long only, max return ######
context("maxret.lo.ROI")

test_that("maxret.lo.ROI contains mean as an objective", 
          { expect_true(maxret.lo.ROI$portfolio$objectives[[1]]$name == "mean") })

test_that("maxret.lo.ROI objective measure mean = 0.008246053", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxret.lo.ROI)$mean), 0.008246053, tolerance=1e-6) })

test_that("maxret.lo.ROI min box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.lo.ROI) >= maxret.lo.ROI$portfolio$constraints[[2]]$min)) })

test_that("maxret.lo.ROI max box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.lo.ROI) <= maxret.lo.ROI$portfolio$constraints[[2]]$max)) })


###### ROI, full_investment, box, max return ######
context("maxret.box.ROI")

test_that("maxret.box.ROI contains mean as an objective", 
          { expect_true(maxret.box.ROI$portfolio$objectives[[1]]$name == "mean") })

test_that("maxret.box.ROI objective measure mean = 0.007508355", 
          { expect_equal(as.numeric(extractObjectiveMeasures(maxret.box.ROI)$mean), 0.007508355, tolerance=1e-6) })

test_that("maxret.box.ROI min box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.box.ROI) >= maxret.box.ROI$portfolio$constraints[[2]]$min)) })

test_that("maxret.lo.ROI max box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.box.ROI) <= maxret.box.ROI$portfolio$constraints[[2]]$max)) })

###### RP, full_investment, box with shorting, max return ######
context("maxret.box1.RP")

test_that("maxret.box1.RP contains StdDev as an objective", 
          { expect_true(maxret.box1.RP$portfolio$objectives[[2]]$name == "StdDev") })

test_that("maxret.box1.RP contains mean as an objective", 
          { expect_true(maxret.box1.RP$portfolio$objectives[[1]]$name == "mean") })

test_that("maxret.box1.RP objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxret.box1.RP)$StdDev)) })

test_that("maxret.box1.RP objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxret.box1.RP)$mean)) })

test_that("maxret.box1.RP min box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.box1.RP) >= maxret.box1.RP$portfolio$constraints[[2]]$min)) })

test_that("minES.box1.RP max box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.box1.RP) <= maxret.box1.RP$portfolio$constraints[[2]]$max)) })

###### RP, full_investment, box, max return ######
context("maxret.box2.RP")

test_that("maxret.box2.RP contains StdDev as an objective", 
          { expect_true(maxret.box2.RP$portfolio$objectives[[2]]$name == "StdDev") })

test_that("maxret.box2.RP contains mean as an objective", 
          { expect_true(maxret.box2.RP$portfolio$objectives[[1]]$name == "mean") })

test_that("maxret.box2.RP objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxret.box2.RP)$StdDev)) })

test_that("maxret.box2.RP objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxret.box2.RP)$mean)) })

test_that("maxret.box2.RP min box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.box2.RP) >= maxret.box2.RP$portfolio$constraints[[2]]$min)) })

test_that("maxret.box2.RP max box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.box2.RP) <= maxret.box2.RP$portfolio$constraints[[2]]$max)) })

###### DE, full_investment, box, max return ######
context("maxret.box.DE")

test_that("maxret.box.DE contains StdDev as an objective", 
          { expect_true(maxret.box.DE$portfolio$objectives[[2]]$name == "StdDev") })

test_that("maxret.box.DE contains mean as an objective", 
          { expect_true(maxret.box.DE$portfolio$objectives[[1]]$name == "mean") })

test_that("maxret.box.DE objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxret.box.DE)$StdDev)) })

test_that("maxret.box.DE objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(maxret.box.DE)$mean)) })

test_that("maxret.box.DE min box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.box.DE) >= maxret.box.DE$portfolio$constraints[[2]]$min)) })

test_that("maxret.box.DE max box constraints are not violated", 
          { expect_true(all(extractWeights(maxret.box.DE) <= maxret.box.DE$portfolio$constraints[[2]]$max)) })
