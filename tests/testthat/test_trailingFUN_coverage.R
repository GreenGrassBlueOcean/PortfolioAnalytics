##### test_trailingFUN_coverage.R #####
# Coverage tests for trailingFUN.R: FUNargs=NULL, bad FUNargs, error/NA path

data(edhec)
R3 <- edhec[1:36, 1:3]

# ===========================================================================
# 1. FUNargs = NULL triggers warning
# ===========================================================================

test_that("trailingFUN warns when FUNargs is NULL", {
  expect_warning(
    trailingFUN(R = 1:50, weights = NULL, n = 10, FUN = "mean", FUNargs = NULL),
    "no FUNargs passed"
  )
})

# ===========================================================================
# 2. Bad FUNargs names trigger pmatch warning
# ===========================================================================

test_that("trailingFUN warns on unmatched FUNargs names", {
  expect_warning(
    trailingFUN(R = 1:50, weights = NULL, n = 10, FUN = "mean",
                FUNargs = list(nonexistent_arg = 42)),
    "do not match"
  )
})

# ===========================================================================
# 3. Function that produces NA triggers message
# ===========================================================================

test_that("trailingFUN handles function returning NA", {
  always_na <- function(R, weights, ...) NA
  expect_message(
    trailingFUN(R = 1:50, weights = NULL, n = 10, FUN = "always_na",
                FUNargs = list()),
    "trailing function generated an error"
  )
})
