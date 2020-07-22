
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

##### ROI Optimization #####
context("demo_group_constraints optimization")

test_that("minStdDev.ROI weights equal c(4.593895e-03, 2.540430e-01, -1.387779e-17, 4.595703e-02, 6.954061e-01)", 
          { expect_equal(as.numeric(extractWeights(minStdDev.ROI)), c(4.593895e-03, 2.540430e-01, -1.387779e-17, 4.595703e-02, 6.954061e-01),
                         tolerance=1e-6) })

test_that("minStdDev.ROI objective measure StdDev = 0.01042408", 
          { expect_equal(as.numeric(extractObjectiveMeasures(minStdDev.ROI)$StdDev), 0.01042408,
                         tolerance=1e-6) })

weights.ROI <- extractWeights(minStdDev.ROI)

test_that("minStdDev.ROI group weights are calculated correctly", 
          { expect_equal(as.numeric(extractGroups(minStdDev.ROI)$group_weights), 
                         c(sum(weights.ROI[c(1, 3, 5)]), sum(weights.ROI[c(2, 4)]))) })

test_that("minStdDev.ROI group constraint cLO is not violated", 
          { expect_true(all(extractGroups(minStdDev.ROI)$group_weights >= cLO)) })

test_that("minStdDev.ROI group constraint cUP is not violated", 
          { expect_true(all(extractGroups(minStdDev.ROI)$group_weights <= cUP)) })


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
          { expect_true(all(extractGroups(minStdDev.RP)$group_weights >= cLO)) })

test_that("minStdDev.RP group constraint cUP is not violated", 
          { expect_true(all(extractGroups(minStdDev.RP)$group_weights <= cUP)) })


##### DE Optimization #####
context("minStdDev.DE")

test_that("minStdDev.DE weights is a numeric vector", 
          { expect_true(is.numeric(extractWeights(minStdDev.DE))) })

test_that("minStdDev.DE objective measure StdDev is numeric", 
          { expect_true(is.numeric(extractObjectiveMeasures(minStdDev.ROI)$StdDev)) })

weights.DE <- extractWeights(minStdDev.DE)

test_that("minStdDev.DE group weights are calculated correctly", 
          { expect_equal(as.numeric(extractGroups(minStdDev.DE)$group_weights), 
                         c(sum(weights.DE[c(1, 3, 5)]), sum(weights.DE[c(2, 4)]))) })

test_that("minStdDev.DE group constraint cLO is not violated", 
          { expect_true(all(extractGroups(minStdDev.DE)$group_weights >= cLO)) })

test_that("minStdDev.DE group constraint cUP is not violated", 
          { expect_true(all(extractGroups(minStdDev.DE)$group_weights <= cUP)) })
