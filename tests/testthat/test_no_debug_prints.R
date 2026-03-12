library(testthat)

context("No unguarded print() calls in package source")

test_that("package is lint-free (print_linter)", {
  skip_if_not_installed("lintr")
  lintr::expect_lint_free()
})
