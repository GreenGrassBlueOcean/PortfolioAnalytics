##### test_local_storage.R #####
# Tests for Proposal #8: Replace global .storage with local environment

library(testthat)
library(PortfolioAnalytics)
library(DEoptim)

context("Local storage for trace accumulation (reentrant)")

# ---- Setup ----

data(edhec)
R <- edhec[1:36, 1:4]
funds <- colnames(R)

portf <- portfolio.spec(assets = funds)
portf <- add.constraint(portf, type = "full_investment")
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
portf <- add.objective(portf, type = "risk", name = "StdDev")


# ---- Unit test: local environment isolation ----

test_that("Two independent storage_env environments do not cross-contaminate", {
  env_a <- new.env(parent = emptyenv())
  env_b <- new.env(parent = emptyenv())
  assign(".objectivestorage", list(), envir = env_a)
  assign(".objectivestorage", list(), envir = env_b)

  w <- rep(0.25, 4)
  names(w) <- funds

  # Write to env_a only
  constrained_objective(w = w, R = R, portfolio = portf, storage_env = env_a)
  constrained_objective(w = w, R = R, portfolio = portf, storage_env = env_a)

  # env_a should have 2 records; env_b should still be empty
  expect_length(get(".objectivestorage", envir = env_a), 2)
  expect_length(get(".objectivestorage", envir = env_b), 0)
})


# ---- Unit test: storage_env=NULL disables storage ----

test_that("constrained_objective with storage_env=NULL does not accumulate", {
  w <- rep(0.25, 4)
  names(w) <- funds
  # With storage_env=NULL (default), should return scalar, no side effects
  val <- constrained_objective(w = w, R = R, portfolio = portf, storage_env = NULL)
  expect_true(is.numeric(val))
  expect_length(val, 1)
})


# ---- Integration: DEoptim trace=TRUE populates results ----

test_that("DEoptim with trace=TRUE populates DEoptim_objective_results", {
  set.seed(3842)
  opt <- suppressMessages(
    optimize.portfolio(R, portf, optimize_method = "DEoptim",
                       search_size = 200, trace = TRUE,
                       itermax = 5, traceDE = FALSE,
                       parallel = FALSE)
  )
  expect_s3_class(opt, "optimize.portfolio.DEoptim")
  expect_true(!is.null(opt$DEoptim_objective_results))
  expect_true(length(opt$DEoptim_objective_results) > 0)

  # Each record should have the expected fields
  rec <- opt$DEoptim_objective_results[[1]]
  expect_true(all(c("out", "weights", "init_weights", "objective_measures") %in% names(rec)))
  expect_true(is.numeric(rec$out))
  expect_true(is.numeric(rec$weights))
  expect_length(rec$weights, length(funds))
})


# ---- Integration: trace=FALSE does NOT produce DEoptim_objective_results ----

test_that("DEoptim with trace=FALSE does not produce DEoptim_objective_results", {
  set.seed(5601)
  opt <- suppressMessages(
    optimize.portfolio(R, portf, optimize_method = "DEoptim",
                       search_size = 200, trace = FALSE,
                       itermax = 5, traceDE = FALSE,
                       parallel = FALSE)
  )
  expect_s3_class(opt, "optimize.portfolio.DEoptim")
  expect_null(opt$DEoptim_objective_results)
})


# ---- No global .storage leakage ----

test_that("No .objectivestorage lingers in package namespace after optimization", {
  set.seed(9017)
  suppressMessages(
    optimize.portfolio(R, portf, optimize_method = "DEoptim",
                       search_size = 200, trace = TRUE,
                       itermax = 5, traceDE = FALSE,
                       parallel = FALSE)
  )
  ns <- asNamespace("PortfolioAnalytics")
  # .objectivestorage should not leak into the namespace
  expect_false(exists(".objectivestorage", envir = ns))
  # If the legacy .storage env exists from a prior load, it should not contain
  # leftover .objectivestorage from this optimization
  if (exists(".storage", envir = ns)) {
    storage_env <- get(".storage", envir = ns)
    expect_false(exists(".objectivestorage", envir = storage_env))
  }
})


# ---- Sequential independence ----

test_that("Two sequential DEoptim trace=TRUE calls produce independent results", {
  set.seed(2197)
  opt1 <- suppressMessages(
    optimize.portfolio(R, portf, optimize_method = "DEoptim",
                       search_size = 200, trace = TRUE,
                       itermax = 5, traceDE = FALSE,
                       parallel = FALSE)
  )
  set.seed(6483)
  opt2 <- suppressMessages(
    optimize.portfolio(R, portf, optimize_method = "DEoptim",
                       search_size = 200, trace = TRUE,
                       itermax = 5, traceDE = FALSE,
                       parallel = FALSE)
  )

  # Both should have results
  expect_true(length(opt1$DEoptim_objective_results) > 0)
  expect_true(length(opt2$DEoptim_objective_results) > 0)

  # Results should be different (different seeds → different populations)
  vals1 <- sapply(opt1$DEoptim_objective_results, `[[`, "out")
  vals2 <- sapply(opt2$DEoptim_objective_results, `[[`, "out")
  # At least some values should differ (extremely unlikely to be identical)
  expect_false(identical(vals1, vals2))
})
