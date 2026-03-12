library(testthat)
library(PortfolioAnalytics)

context("optimization_failure S3 class")

test_that("optimization_failure constructor returns correct class", {
  obj <- optimization_failure(
    message = "test failure",
    solver  = "DEoptim"
  )
  expect_s3_class(obj, "optimization_failure")
  expect_s3_class(obj, "optimize.portfolio")
  expect_true(is.optimization_failure(obj))
})

test_that("optimization_failure stores all fields", {
  fake_call <- quote(optimize.portfolio(R, spec))
  fake_error <- simpleError("solver blew up")

  obj <- optimization_failure(
    message = "no solution found",
    solver  = "GenSA",
    call    = fake_call,
    error   = fake_error
  )

  expect_equal(obj$message, "no solution found")
  expect_equal(obj$solver, "GenSA")
  expect_identical(obj$call, fake_call)
  expect_identical(obj$error, fake_error)
  expect_true(is.na(obj$weights))
  expect_true(is.na(obj$objective_measures))
  expect_true(is.na(obj$opt_values))
  expect_true(is.na(obj$elapsed_time))
})

test_that("is.optimization_failure returns FALSE for non-failure objects", {
  expect_false(is.optimization_failure(list(weights = 1)))
  expect_false(is.optimization_failure("a string"))
  expect_false(is.optimization_failure(NULL))
})

test_that("print method runs without error", {
  obj <- optimization_failure(
    message = "solver failed",
    solver  = "pso"
  )
  expect_output(print(obj), "Optimization Failure")
  expect_output(print(obj), "pso")
})

test_that("$ access on optimization_failure returns expected values, not NULL", {
  obj <- optimization_failure(
    message = "solver failed",
    solver  = "DEoptim"
  )
  # The key behavioral change: $weights returns NA (not NULL as it would

  # on a character string), and $message is accessible.
  expect_true(is.na(obj$weights))
  expect_equal(obj$message, "solver failed")
})
