
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####

context("test backwards_compat.R")


test_that("backwards_compat.R runs succesfully", {
  expect_error(source(system.file("demo/backwards_compat.R", package="PortfolioAnalytics")), NA)
})


context("Backwards compatibility is maintained")

# class
test_that("Class of gen.constr is v1_constraint", 
          { expect_true(inherits(gen.constr, "v1_constraint")) })

# assets
test_that("Initial assets form an equal weight portfolio", 
          { expect_true(all.equal(as.numeric(gen.constr$assets), rep(1/4, 4))) })

# min
test_that("Box constraints min vector is all 0s", 
          { expect_true(all.equal(as.numeric(gen.constr$min), rep(0, 4))) })

# max
test_that("Box constraints max vector is all 0.55", 
          { expect_true(all.equal(as.numeric(gen.constr$max), rep(0.55, 4))) })

# min_mult
test_that("min_mult is null", 
          { expect_true(is.null(gen.constr$min_mult)) })

# max_mult
test_that("max_mult is null", 
          { expect_true(is.null(gen.constr$max_mult)) })

# min_sum
test_that("min_sum is 0.99", 
          { expect_true(all.equal(gen.constr$min_sum, 0.99)) })

# max_sum
test_that("min_sum is 1.01", 
          { expect_true(all.equal(gen.constr$max_sum, 1.01)) })

# mean objective
test_that("The objective name is 'mean'", 
          { expect_true(all.equal(gen.constr$objectives[[1]]$name, "mean")) })

context("Optimization output")

# Not sure how to test for exact values of optimization results for DEoptim
# and random portfolios
# - use a specific data set of rp weights

# random portfolios optimization
test_that("random portfolios updated portfolio object", 
          { expect_true(inherits(optrpv1$portfolio, "portfolio.spec")) })

test_that("random portfolios returns optimal weights", 
          { expect_true(is.numeric(extractWeights(optrpv1))) })

test_that("random portfolios returns an objective measure", 
          { expect_true(is.numeric(extractObjectiveMeasures(optrpv1)$mean)) })

# DEoptim optimization
test_that("DE optim updated portfolio object", 
          { expect_true(inherits(optdev1$portfolio, "portfolio.spec")) })

test_that("DE optim returns optimal weights", 
          { expect_true(is.numeric(extractWeights(optdev1))) })

test_that("DE optim returns an objective measure", 
          { expect_true(is.numeric(extractObjectiveMeasures(optdev1)$mean)) })

# ROI optimization
test_that("ROI updated portfolio object", 
          { expect_true(inherits(optroiv1$portfolio, "portfolio.spec")) })

test_that("ROI returns optimal weights equal to c(0, 0, 0.46, 0.55)", 
          { expect_equal(as.numeric(extractWeights(optroiv1)), c(0, 0, 0.46, 0.55)) })

test_that("ROI returns an objective measure mean=0.008193842", 
          { expect_equal(as.numeric(extractObjectiveMeasures(optroiv1)$mean), 0.008193842) })

