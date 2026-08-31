# Regression tests for the optional-argument guards in the DEoptim branch of
# optimize.portfolio().
#
# Optional controls there are read with
#     if (hasArg(x)) x <- eval.parent(match.call(expand.dots = TRUE)$x)
# and were then tested with is.na(x). Because hasArg() is TRUE for an argument
# passed explicitly as NULL, that test failed in three distinct ways:
#
#   1. itermax: ifelse(is.na(NULL), ...) gave logical(0), so
#      NP <- round(search_size/itermax) became numeric(0) and
#      `if (NP < (N * 10))` threw "argument is of length zero" -- before
#      DEoptim was ever invoked.
#   2. strategy / reltol / steptol / c / storepopfrom: in the
#      `!hasArg(x) || is.na(...)` guards the same logical(0) made the `||`
#      evaluate to NA, so `if (NA)` threw
#      "missing value where TRUE/FALSE needed".
#   3. packages: is.na() on a character vector returns a vector and since
#      R 4.3 `||` errors with "'length = 2' in coercion to 'logical(1)'",
#      making that documented argument impossible to supply.
#
# Omitting an argument always worked; passing NULL was fatal. They now mean
# the same thing.

test_that(".pa_arg_missing treats NULL, empty and scalar NA as not supplied", {
  f <- .pa_arg_missing

  expect_true(f(NULL))
  expect_true(f(numeric(0)))
  expect_true(f(character(0)))
  expect_true(f(logical(0)))
  expect_true(f(NA))
  expect_true(f(NA_integer_))
  expect_true(f(NA_character_))
})

test_that(".pa_arg_missing treats real values as supplied", {
  f <- .pa_arg_missing

  expect_false(f(15L))
  expect_false(f(200))
  expect_false(f(0))
  expect_false(f(TRUE))
  expect_false(f(FALSE))
  expect_false(f("sample"))
  # Vectors count as supplied -- this is the `packages` case that used to make
  # `||` error outright.
  expect_false(f(c("xts", "zoo")))
  expect_false(f(1:5))
  expect_false(f(list(a = 1)))
})

test_that("the old guard shapes really did fail", {
  # Shape 1: the itermax sizing block.
  old_itermax_block <- function(search_size, itermax, N) {
    itermax <- ifelse(is.na(itermax), yes = TRUE, no = itermax)
    NP <- round(search_size / itermax)
    if (NP < (N * 10)) NP <- N * 10
    NP
  }
  expect_error(old_itermax_block(1000, NULL, 4), "argument is of length zero")

  # Shape 2: `!hasArg(x) || is.na(...)` with an explicit NULL.
  expect_error(
    if (FALSE || is.na(NULL)) "default" else "supplied",
    "missing value where TRUE/FALSE needed"
  )

  # Shape 3: a vector argument such as `packages`.
  expect_error(
    if (FALSE || is.na(c("xts", "zoo"))) "default" else "supplied",
    "length = 2"
  )
})

test_that("the new guard resolves all three cases", {
  f <- .pa_arg_missing
  resolve <- function(arg, default) if (f(arg)) default else arg

  expect_equal(resolve(NULL, 200), 200)
  expect_equal(resolve(NA, 200), 200)
  expect_equal(resolve(15L, 200), 15L)
  expect_equal(resolve(c("xts", "zoo"), "all"), c("xts", "zoo"))

  np <- function(search_size, itermax, N) {
    supplied <- !f(itermax)
    itermax <- if (supplied) itermax else N * 50
    NP <- round(search_size / itermax)
    if (NP < (N * 10)) NP <- N * 10
    if (NP > 2000) NP <- 2000
    NP
  }
  for (it in list(NULL, NA, 15L, 200, 50L)) {
    expect_length(np(1000, it, 4), 1L)
  }
})

test_that("optimize.portfolio survives an explicit itermax = NULL", {
  skip_if_not_installed("DEoptim")

  data("edhec")
  R <- edhec[, 1:3]

  port <- portfolio.spec(assets = colnames(R))
  port <- add.constraint(port, type = "weight_sum",
                         min_sum = 0.99, max_sum = 1.01)
  port <- add.constraint(port, type = "box", min = 0, max = 1)
  port <- add.objective(port, type = "risk", name = "StdDev")

  # Before the fix this died with "argument is of length zero" before DEoptim
  # was invoked. It must now behave exactly as omitting itermax does.
  expect_no_error(
    suppressWarnings(suppressMessages(
      optimize.portfolio(
        R               = R,
        portfolio       = port,
        optimize_method = "DEoptim",
        search_size     = 200,
        trace           = FALSE,
        traceDE         = 0,
        itermax         = NULL
      )
    ))
  )
})
