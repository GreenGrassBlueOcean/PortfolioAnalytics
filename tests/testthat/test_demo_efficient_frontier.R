
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

context("test demo_efficient_frontier.R")

##### Source Demo Script #####

test_that("demo_efficient_frontier.R runs succesfully", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(source(system.file("demo/demo_efficient_frontier.R", package="PortfolioAnalytics")), NA)
})


context("mean-var efficient frontier")

test_that("meanvar.ef$frontier has 25 rows", 
          { expect_equal(nrow(meanvar.ef$frontier), 25) })

test_that("colnames(meanvar.ef$frontier) are consistent", 
          { expect_equal(colnames(meanvar.ef$frontier), c("mean", "StdDev", "out", "w.CA", "w.CTAG", "w.DS", "w.EM", "w.EQM")) })

test_that("meanvar.ef weights are feasible and sum to ~1", {
  wt_cols <- grep("^w\\.", colnames(meanvar.ef$frontier), value = TRUE)
  weights <- meanvar.ef$frontier[, wt_cols]
  # Weights within box constraints (0.15 to 0.45, relaxed slightly for solver tolerance)
  expect_true(all(weights >= 0.15 - 1e-6))
  expect_true(all(weights <= 0.45 + 1e-6))
  # Each row sums to ~1
  row_sums <- rowSums(weights)
  expect_true(all(abs(row_sums - 1) < 0.02))
})

test_that("meanvar.ef frontier is monotonically non-decreasing in mean and StdDev", {
  means <- meanvar.ef$frontier[, "mean"]
  sds   <- meanvar.ef$frontier[, "StdDev"]
  # Both should be non-decreasing along the frontier (higher return = higher risk)
  expect_true(all(diff(means) >= -1e-10))
  expect_true(all(diff(sds) >= -1e-10))
})

test_that("meanvar.ef frontier values are in plausible range", {
  means <- meanvar.ef$frontier[, "mean"]
  sds   <- meanvar.ef$frontier[, "StdDev"]
  expect_true(all(means > 0 & means < 0.05))
  expect_true(all(sds > 0 & sds < 0.10))
})

context("mean-etl efficient frontier")

test_that("meanetl.ef$frontier has 25 rows", 
          { expect_equal(nrow(meanetl.ef$frontier), 25) })

test_that("colnames(meanetl.ef$frontier) are consistent", {
  # braverock's efficient frontier puts risk column first
  expect_true(setequal(
    colnames(meanetl.ef$frontier),
    c("mean", "ES", "out", "w.CA", "w.CTAG", "w.DS", "w.EM", "w.EQM")
  ))
})

test_that("meanetl.ef weights are feasible and sum to ~1", {
  wt_cols <- grep("^w\\.", colnames(meanetl.ef$frontier), value = TRUE)
  weights <- meanetl.ef$frontier[, wt_cols]
  # CVXR SCS solver has ~1% constraint tolerance
  expect_true(all(weights >= 0.15 - 0.01))
  expect_true(all(weights <= 0.45 + 0.01))
  row_sums <- rowSums(weights)
  expect_true(all(abs(row_sums - 1) < 0.05))
})

test_that("meanetl.ef frontier is monotonically non-decreasing in mean and ES", {
  means <- meanetl.ef$frontier[, "mean"]
  es    <- meanetl.ef$frontier[, "ES"]
  # Allow small tolerance for CVXR solver numerical noise
  expect_true(all(diff(means) >= -1e-4))
  expect_true(all(diff(es) >= -1e-4))
})

test_that("meanetl.ef frontier values are in plausible range", {
  means <- meanetl.ef$frontier[, "mean"]
  es    <- meanetl.ef$frontier[, "ES"]
  expect_true(all(means > 0 & means < 0.05))
  expect_true(all(es > 0 & es < 0.20))
})
