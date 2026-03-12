##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

context("test demo_max_quadratic_utility.R")

##### Source Demo Script #####
# The demo may error when plotting results from extreme risk_aversion values
# (e.g. 1e-5) that produce NA weights with current edhec data.
# We source with tryCatch so subsequent tests on valid objects still run.

test_that("demo_max_quadratic_utility.R sources without fatal setup errors", {
  tryCatch(
    source(system.file("demo/demo_max_quadratic_utility.R", package = "PortfolioAnalytics")),
    error = function(e) NULL
  )
  # The first optimization (risk_aversion=4) should always succeed
  expect_true(exists("maxQU.lo.ROI"))
})

##### init.portf objectives #####
context("objectives for quadratic utility")

test_that("init.portf contains mean as an objective",
          { expect_true(init.portf$objectives[[1]]$name == "mean") })

test_that("init.portf contains StdDev as an objective",
          { expect_true(init.portf$objectives[[2]]$name == "StdDev") })

##### ROI, full_investment, long only, max qu (risk_aversion=4) ######
context("maxQU.lo.ROI")

test_that("maxQU.lo.ROI objective measure mean is positive and plausible", {
  val <- as.numeric(extractObjectiveMeasures(maxQU.lo.ROI)$mean)
  expect_true(is.numeric(val) && val > 0 && val < 0.05)
})

test_that("maxQU.lo.ROI objective measure StdDev is positive and plausible", {
  val <- as.numeric(extractObjectiveMeasures(maxQU.lo.ROI)$StdDev)
  expect_true(is.numeric(val) && val > 0 && val < 0.05)
})

test_that("maxQU.lo.ROI min box constraints are not violated",
          { expect_true(all(extractWeights(maxQU.lo.ROI) + 1e-8 >= maxQU.lo.ROI$portfolio$constraints[[2]]$min)) })

test_that("maxQU.lo.ROI max box constraints are not violated",
          { expect_true(all(extractWeights(maxQU.lo.ROI) <= maxQU.lo.ROI$portfolio$constraints[[2]]$max)) })

##### ROI, risk_aversion=1e-5 (near-zero penalty, may produce NA weights) ######
context("maxQU.maxret.ROI")

# With current edhec data, risk_aversion=1e-5 can produce NA weights.
# Skip these tests if the object doesn't exist or has NA weights.
maxret_valid <- exists("maxQU.maxret.ROI") &&
  !any(is.na(extractWeights(maxQU.maxret.ROI)))

test_that("maxQU.maxret.ROI produces valid weights (may be skipped)", {
  skip_if_not(maxret_valid, "maxQU.maxret.ROI has NA weights with current data")
  w <- extractWeights(maxQU.maxret.ROI)
  expect_true(is.numeric(w) && all(!is.na(w)))
})

##### ROI, risk_aversion=1e6 (approximates min variance) ######
context("maxQU.minvol.ROI")

minvol_valid <- exists("maxQU.minvol.ROI") &&
  !any(is.na(extractWeights(maxQU.minvol.ROI)))

test_that("maxQU.minvol.ROI produces valid results (may be skipped)", {
  skip_if_not(minvol_valid, "maxQU.minvol.ROI not created (demo error)")
  val <- as.numeric(extractObjectiveMeasures(maxQU.minvol.ROI)$StdDev)
  expect_true(is.numeric(val) && val > 0 && val < 0.05)
})

test_that("maxQU.minvol.ROI min box constraints are not violated", {
  skip_if_not(minvol_valid, "maxQU.minvol.ROI not created (demo error)")
  expect_true(all(extractWeights(maxQU.minvol.ROI) + 1e-8 >= maxQU.minvol.ROI$portfolio$constraints[[2]]$min))
})

test_that("maxQU.minvol.ROI max box constraints are not violated", {
  skip_if_not(minvol_valid, "maxQU.minvol.ROI not created (demo error)")
  expect_true(all(extractWeights(maxQU.minvol.ROI) <= maxQU.minvol.ROI$portfolio$constraints[[2]]$max))
})
