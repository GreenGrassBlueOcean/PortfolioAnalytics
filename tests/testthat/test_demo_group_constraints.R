
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

context("test demo_group_constraints.R")


##### Source Demo Script #####


test_that("demo_group_constraints.R runs succesfully", {
  expect_error(source(system.file("demo/demo_group_constraints.R", package="PortfolioAnalytics")), NA)
})

##### Test the constraints #####

group_constr <- init.portf$constraints[[3]]

test_that("init.portf contains groups as a constraint", 
          { expect_true(inherits(group_constr, "group_constraint")) })

test_that("group constraint for groupA  is c(1, 3, 5)", 
          { expect_equal(group_constr$groups$groupA, c(1, 3, 5)) })

test_that("group constraint for groupB  is c(2, 4)", 
          { expect_equal(group_constr$groups$groupB, c(2, 4)) })

test_that("group constraint cLO  is c(0.05, 0.15)", 
          { expect_equal(group_constr$cLO, c(0.05, 0.15)) })

test_that("group constraint cUP  is c(0.7, 0.5)", 
          { expect_equal(group_constr$cUP, c(0.7, 0.5)) })

cLO <- group_constr$cLO
cUP <- group_constr$cUP

# Tolerance for floating point comparisons at constraint boundaries.
# Solvers may land exactly on a bound, producing machine-epsilon violations.
fp_tol <- 1e-8

##### ROI Optimization #####
context("demo_group_constraints optimization")

test_that("minStdDev.ROI returns named numeric weights that sum to 1", {
  w <- extractWeights(minStdDev.ROI)
  expect_true(is.numeric(w))
  expect_equal(length(w), 5)
  expect_equal(names(w), c("CA", "CTAG", "DS", "EM", "EQM"))
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

test_that("minStdDev.ROI weights satisfy long-only constraint", {
  w <- extractWeights(minStdDev.ROI)
  expect_true(all(w >= -fp_tol))
})

test_that("minStdDev.ROI objective measure StdDev is positive and plausible", {
  sd_val <- as.numeric(extractObjectiveMeasures(minStdDev.ROI)$StdDev)
  expect_true(sd_val > 0)
  expect_true(sd_val < 0.05)
})

weights.ROI <- extractWeights(minStdDev.ROI)

test_that("minStdDev.ROI group weights are calculated correctly", 
          { expect_equal(as.numeric(extractGroups(minStdDev.ROI)$group_weights), 
                         c(sum(weights.ROI[c(1, 3, 5)]), sum(weights.ROI[c(2, 4)]))) })

test_that("minStdDev.ROI group constraint cLO is not violated", 
          { expect_true(all(extractGroups(minStdDev.ROI)$group_weights >= cLO - fp_tol)) })

test_that("minStdDev.ROI group constraint cUP is not violated", 
          { expect_true(all(extractGroups(minStdDev.ROI)$group_weights <= cUP + fp_tol)) })


##### RP Optimization #####
context("minStdDev.RP")

test_that("minStdDev.RP weights is a numeric vector", 
          { expect_true(is.numeric(extractWeights(minStdDev.RP))) })

test_that("minStdDev.RP objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minStdDev.RP)$StdDev)) })

weights.RP <- extractWeights(minStdDev.RP)

test_that("minStdDev.RP group weights are calculated correctly", 
          { expect_equal(as.numeric(extractGroups(minStdDev.RP)$group_weights), 
                         c(sum(weights.RP[c(1, 3, 5)]), sum(weights.RP[c(2, 4)]))) })

test_that("minStdDev.RP group constraint cLO is not violated", 
          { expect_true(all(extractGroups(minStdDev.RP)$group_weights >= cLO - fp_tol)) })

test_that("minStdDev.RP group constraint cUP is not violated", 
          { expect_true(all(extractGroups(minStdDev.RP)$group_weights <= cUP + fp_tol)) })


##### DE Optimization #####
context("minStdDev.DE")

test_that("minStdDev.DE weights is a numeric vector", 
          { expect_true(is.numeric(extractWeights(minStdDev.DE))) })

test_that("minStdDev.DE objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minStdDev.DE)$StdDev)) })

weights.DE <- extractWeights(minStdDev.DE)

test_that("minStdDev.DE group weights are calculated correctly", 
          { expect_equal(as.numeric(extractGroups(minStdDev.DE)$group_weights), 
                         c(sum(weights.DE[c(1, 3, 5)]), sum(weights.DE[c(2, 4)]))) })

test_that("minStdDev.DE group constraint cLO is not violated", 
          { expect_true(all(extractGroups(minStdDev.DE)$group_weights >= cLO - fp_tol)) })

test_that("minStdDev.DE group constraint cUP is not violated", 
          { expect_true(all(extractGroups(minStdDev.DE)$group_weights <= cUP + fp_tol)) })
