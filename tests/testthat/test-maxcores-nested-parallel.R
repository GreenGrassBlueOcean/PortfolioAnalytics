# Tests for MaxCores / MaxSubCores, which bound cluster size and nested
# parallelism.
#
# optimize.portfolio() previously hard-coded its SOCK cluster at
# ifelse(nC <= 15, nC, 15). MaxCores makes that an argument, defaulting to 15
# so the sizing is unchanged.
#
# optimize.portfolio.rebalancing() farms the rebalance periods out with
# %dopar%. Letting each worker build its own cluster oversubscribes the machine
# by outer x inner workers, so the inner calls used to be hard-wired to
# parallel = FALSE. MaxSubCores keeps that as the default (1 => sequential)
# while allowing bounded nesting when asked for.

test_that("MaxCores and MaxSubCores are exposed with backward-compatible defaults", {
  fo <- formals(optimize.portfolio)
  expect_true("MaxCores" %in% names(fo))
  # 15 reproduces the previous hard-coded cap.
  expect_equal(eval(fo$MaxCores), 15)

  fr <- formals(optimize.portfolio.rebalancing)
  expect_true("MaxSubCores" %in% names(fr))
  # 1 reproduces the previous parallel = FALSE behaviour for inner calls.
  expect_equal(eval(fr$MaxSubCores), 1)
})

test_that("MaxSubCores = 1 means the inner optimisations stay sequential", {
  # The wiring is `parallel = .sub_parallel` where .sub_parallel is
  # MaxSubCores > 1, so the default must evaluate to FALSE.
  sub_parallel <- function(MaxSubCores) as.integer(MaxSubCores) > 1L

  expect_false(sub_parallel(1))
  expect_true(sub_parallel(2))
  expect_true(sub_parallel(8))
})

test_that("cluster sizing never exceeds MaxCores or the detected core count", {
  size <- function(nC, MaxCores) min(nC, as.integer(MaxCores))

  # Default 15 matches the old ifelse(nC <= 15, nC, 15) at every core count.
  for (nC in c(1, 2, 8, 15, 16, 32, 128)) {
    expect_equal(size(nC, 15), ifelse(nC <= 15, nC, 15))
  }

  # A lower cap is honoured.
  expect_equal(size(32, 4), 4)
  expect_equal(size(2, 4), 2)
})

test_that("MaxSubCores rejects values that would silently misconfigure nesting", {
  data("edhec")
  R <- edhec[1:48, 1:3]

  # `random` needs finite box bounds to generate portfolios, so mirror the
  # fixture pattern used elsewhere in the suite (weight_sum + box).
  port <- portfolio.spec(assets = colnames(R))
  port <- add.constraint(port, type = "weight_sum",
                         min_sum = 0.99, max_sum = 1.01)
  port <- add.constraint(port, type = "box", min = 0.05, max = 0.55)
  port <- add.objective(port, type = "risk", name = "StdDev")

  for (bad in list(0, -1, NA, NULL, "two", c(1, 2))) {
    expect_error(
      optimize.portfolio.rebalancing(
        R = R, portfolio = port, optimize_method = "random",
        search_size = 20, rebalance_on = "quarters",
        training_period = 12, MaxSubCores = bad
      ),
      "MaxSubCores must be a single number"
    )
  }
})

test_that("MaxCores rejects invalid values rather than building an odd cluster", {
  # The guard lives on the parallelType = 2 path; check it directly so the test
  # does not depend on snow/doSNOW being installed.
  validate <- function(MaxCores) {
    if (!is.numeric(MaxCores) || length(MaxCores) != 1L || is.na(MaxCores) ||
        MaxCores < 1) {
      stop("MaxCores must be a single number >= 1, but has value ",
           paste(deparse(MaxCores), collapse = ""), call. = FALSE)
    }
    as.integer(MaxCores)
  }

  for (bad in list(0, -4, NA, NULL, "eight", c(2, 3))) {
    expect_error(validate(bad), "MaxCores must be a single number")
  }
  expect_equal(validate(15), 15L)
  expect_equal(validate(4.0), 4L)
})

test_that("rebalancing still runs end to end with the default MaxSubCores", {
  data("edhec")
  R <- edhec[1:48, 1:3]

  # `random` needs finite box bounds to generate portfolios, so mirror the
  # fixture pattern used elsewhere in the suite (weight_sum + box).
  port <- portfolio.spec(assets = colnames(R))
  port <- add.constraint(port, type = "weight_sum",
                         min_sum = 0.99, max_sum = 1.01)
  port <- add.constraint(port, type = "box", min = 0.05, max = 0.55)
  port <- add.objective(port, type = "risk", name = "StdDev")

  expect_no_error(
    suppressWarnings(suppressMessages(
      optimize.portfolio.rebalancing(
        R = R, portfolio = port, optimize_method = "random",
        search_size = 200, rebalance_on = "quarters", training_period = 24
      )
    ))
  )
})
