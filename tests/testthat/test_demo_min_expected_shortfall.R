
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####

context("test demo_min_expected_shortfall.R")

test_that("demo_min_expected_shortfall.R runs succesfully", {
  expect_error(source(system.file("demo/demo_min_expected_shortfall.R", package="PortfolioAnalytics")), NA)
})

###### ROI, full_investment, long only, min ES ######
context("minES.lo.ROI")

test_that("minES.lo.ROI contains ES as an objective", 
          { expect_true(minES.lo.ROI$portfolio$objectives[[1]]$name == "ES") })

test_that("minES.lo.ROI ES objective p=0.9", 
          { expect_equal(minES.lo.ROI$portfolio$objectives[[1]]$arguments$p, 0.9) })

test_that("minES.lo.ROI objective measure ES = 0.01013571", 
          { expect_equal(extractObjectiveMeasures(minES.lo.ROI)$ES, 0.01013571, tolerance=1e-6) })

test_that("minES.lo.ROI min box constraints are not violated", 
          { expect_true(all(extractWeights(minES.lo.ROI) >= minES.lo.ROI$portfolio$constraints[[2]]$min)) })

test_that("minES.lo.ROI max box constraints are not violated", 
          { expect_true(all(extractWeights(minES.lo.ROI) <= minES.lo.ROI$portfolio$constraints[[2]]$max)) })

###### ROI, full_investment, long only, min ES ######
context("minES.box.ROI")

test_that("minES.box.ROI contains ES as an objective", 
          { expect_true(minES.box.ROI$portfolio$objectives[[1]]$name == "ES") })

test_that("minES.box.ROI ES objective p=0.9", 
          { expect_equal(minES.box.ROI$portfolio$objectives[[1]]$arguments$p, 0.9) })

test_that("minES.box.ROI objective measure ES = 0.01477709", 
          { expect_equal(as.numeric(extractObjectiveMeasures(minES.box.ROI)$ES), 0.01477709, tolerance=1e-6) })

test_that("minES.box.ROI min box constraints are not violated", 
          { expect_true(all(extractWeights(minES.box.ROI) >= minES.box.ROI$portfolio$constraints[[2]]$min)) })

test_that("minES.box.ROI max box constraints are not violated", 
          { expect_true(all(extractWeights(minES.box.ROI) <= minES.box.ROI$portfolio$constraints[[2]]$max)) })

###### RP, full_investment, box, min ES ######
context("minES.box1.RP")

test_that("minES.box1.RP contains ES as an objective", 
          { expect_true(minES.box1.RP$portfolio$objectives[[1]]$name == "ES") })

test_that("minES.box1.RP ES objective p=0.9", 
          { expect_equal(minES.box1.RP$portfolio$objectives[[1]]$arguments$p, 0.9) })

test_that("minES.box1.RP contains mean as an objective", 
          { expect_true(minES.box1.RP$portfolio$objectives[[2]]$name == "mean") })

test_that("minES.box1.RP objective measure ES is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minES.box1.RP)$ES)) })

test_that("minES.box1.RP objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minES.box1.RP)$mean)) })

test_that("minES.box1.RP min box constraints are not violated", 
          { expect_true(all(extractWeights(minES.box1.RP) >= minES.box1.RP$portfolio$constraints[[2]]$min)) })

test_that("minES.box1.RP max box constraints are not violated", 
          { expect_true(all(extractWeights(minES.box1.RP) <= minES.box1.RP$portfolio$constraints[[2]]$max)) })

###### RP, full_investment, box, min ES ######
context("minES.box2.RP")

test_that("minES.box2.RP contains ES as an objective", 
          { expect_true(minES.box2.RP$portfolio$objectives[[1]]$name == "ES") })

test_that("minES.box2.RP ES objective p=0.9", 
          { expect_equal(minES.box2.RP$portfolio$objectives[[1]]$arguments$p, 0.9) })

test_that("minES.box2.RP contains mean as an objective", 
          { expect_true(minES.box2.RP$portfolio$objectives[[2]]$name == "mean") })

test_that("minES.box2.RP objective measure ES is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minES.box2.RP)$ES)) })

test_that("minES.box2.RP objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minES.box2.RP)$mean)) })

test_that("minES.box2.RP min box constraints are not violated", 
          { expect_true(all(extractWeights(minES.box2.RP) >= minES.box2.RP$portfolio$constraints[[2]]$min)) })

test_that("minES.box2.RP max box constraints are not violated", 
          { expect_true(all(extractWeights(minES.box2.RP) <= minES.box2.RP$portfolio$constraints[[2]]$max)) })

###### DE, full_investment, box, min ES ######
context("minES.box1.DE")

test_that("minES.box.DE contains ES as an objective", 
          { expect_true(minES.box.DE$portfolio$objectives[[1]]$name == "ES") })

test_that("minES.box.DE ES objective p=0.9", 
          { expect_equal(minES.box.DE$portfolio$objectives[[1]]$arguments$p, 0.9) })

test_that("minES.box2.DE contains mean as an objective", 
          { expect_true(minES.box.DE$portfolio$objectives[[2]]$name == "mean") })

test_that("minES.box.DE objective measure ES is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minES.box.DE)$ES)) })

test_that("minES.box.DE objective measure mean is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minES.box.DE)$mean)) })

test_that("minES.box.DE min box constraints are not violated", 
          { expect_true(all(extractWeights(minES.box.DE) >= minES.box.DE$portfolio$constraints[[2]]$min)) })

test_that("minES.box.DE max box constraints are not violated", 
          { expect_true(all(extractWeights(minES.box.DE) <= minES.box.DE$portfolio$constraints[[2]]$max)) })


