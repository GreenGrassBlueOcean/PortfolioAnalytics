##### test_utility_combine_coverage.R #####
# Coverage: utility.combine.R — error path validation

test_that("combine.optimizations rejects non-list input", {
  expect_error(combine.optimizations("not a list"), "must be passed in as a list")
  expect_error(combine.optimizations(42), "must be passed in as a list")
})

test_that("combine.optimizations rejects invalid class in list", {
  p <- portfolio.spec(assets = c("A", "B"))
  expect_error(
    combine.optimizations(list(p, "invalid")),
    "optimize.portfolio"
  )
})

test_that("combine.portfolios rejects non-list input", {
  expect_error(combine.portfolios("not a list"), "must be passed in as a list")
  expect_error(combine.portfolios(123), "must be passed in as a list")
})

test_that("combine.portfolios rejects invalid class in list", {
  p <- portfolio.spec(assets = c("A", "B"))
  expect_error(
    combine.portfolios(list(p, "not a portfolio")),
    "class 'portfolio'"
  )
})
