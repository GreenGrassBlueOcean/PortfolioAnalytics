##### test_v1_deprecation.R #####
# Tests for Proposal #7: v1 function deprecation warnings

library(testthat)
library(PortfolioAnalytics)

context("v1 API deprecation warnings")

# ---- Setup ----

data(edhec)
R <- edhec[1:36, 1:4]
funds <- colnames(R)

# Reset once-per-session deprecation state so tests are independent.
# The .deprecation_warned env lives in the package namespace.
reset_deprecation_state <- function() {
  env <- get(".deprecation_warned",
             envir = asNamespace("PortfolioAnalytics"))
  rm(list = ls(env), envir = env)
}


# ---- constraint_v1 ----

test_that("constraint_v1() emits deprecation warning", {
  expect_warning(
    constraint_v1(assets = funds, min = 0, max = 0.55,
                  min_sum = 0.99, max_sum = 1.01),
    "deprecated"
  )
})

test_that("constraint_v1() still returns a v1_constraint object", {
  obj <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 0.55,
                  min_sum = 0.99, max_sum = 1.01)
  )
  expect_s3_class(obj, "v1_constraint")
})


# ---- add.objective_v1 ----

test_that("add.objective_v1() emits deprecation warning", {
  constr <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 0.55,
                  min_sum = 0.99, max_sum = 1.01)
  )
  expect_warning(
    add.objective_v1(constraints = constr, type = "return", name = "mean"),
    "deprecated"
  )
})

test_that("add.objective_v1() still adds the objective", {
  constr <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 0.55,
                  min_sum = 0.99, max_sum = 1.01)
  )
  constr2 <- suppressWarnings(
    add.objective_v1(constraints = constr, type = "return", name = "mean")
  )
  expect_length(constr2$objectives, 1)
  expect_equal(constr2$objectives[[1]]$name, "mean")
})


# ---- optimize.portfolio_v1 ----

test_that("optimize.portfolio_v1() emits deprecation warning", {
  reset_deprecation_state()
  constr <- suppressWarnings({
    c1 <- constraint_v1(assets = funds, min = 0, max = 0.55,
                        min_sum = 0.99, max_sum = 1.01,
                        weight_seq = generatesequence(min = 0, max = 0.55, by = 0.01))
    add.objective_v1(constraints = c1, type = "return", name = "mean")
  })
  expect_warning(
    optimize.portfolio_v1(R = R, constraints = constr,
                          optimize_method = "random",
                          search_size = 20),
    "deprecated"
  )
  reset_deprecation_state()
})


# ---- optimize.portfolio() with v1_constraint auto-conversion ----

test_that("optimize.portfolio() with v1_constraint emits deprecation warning", {
  constr <- suppressWarnings({
    c1 <- constraint_v1(assets = funds, min = 0, max = 0.55,
                        min_sum = 0.99, max_sum = 1.01)
    add.objective_v1(constraints = c1, type = "return", name = "mean")
  })
  expect_warning(
    optimize.portfolio(R = R, constraints = constr,
                       optimize_method = "random",
                       search_size = 20),
    "deprecated"
  )
})

test_that("optimize.portfolio() with v1_constraint still produces results", {
  constr <- suppressWarnings({
    c1 <- constraint_v1(assets = funds, min = 0, max = 0.55,
                        min_sum = 0.99, max_sum = 1.01)
    add.objective_v1(constraints = c1, type = "return", name = "mean")
  })
  opt <- suppressWarnings(
    optimize.portfolio(R = R, constraints = constr,
                       optimize_method = "random",
                       search_size = 20)
  )
  expect_s3_class(opt, "optimize.portfolio")
  expect_true(is.numeric(extractWeights(opt)))
})


# ---- constrained_objective_v1 (once-per-session) ----

test_that("constrained_objective_v1() emits deprecation warning once", {
  reset_deprecation_state()
  constr <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 0.55,
                  min_sum = 0.99, max_sum = 1.01,
                  weight_seq = generatesequence(min = 0, max = 0.55, by = 0.01))
  )
  constr <- suppressWarnings(
    add.objective_v1(constraints = constr, type = "return", name = "mean")
  )

  w <- rep(1 / length(funds), length(funds))
  names(w) <- funds

  # First call should warn
  expect_warning(
    constrained_objective_v1(w = w, R = R, constraints = constr),
    "deprecated"
  )
  # Second call should NOT warn (once-per-session)
  expect_silent(
    constrained_objective_v1(w = w, R = R, constraints = constr)
  )
  reset_deprecation_state()
})


# ---- randomize_portfolio_v1 (once-per-session) ----

test_that("randomize_portfolio_v1() emits deprecation warning once", {
  reset_deprecation_state()
  constr <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 0.55,
                  min_sum = 0.99, max_sum = 1.01,
                  weight_seq = generatesequence(min = 0, max = 0.55, by = 0.01))
  )
  # First call should warn
  expect_warning(
    randomize_portfolio_v1(rpconstraints = constr),
    "deprecated"
  )
  # Second call should NOT warn
  expect_silent(
    randomize_portfolio_v1(rpconstraints = constr)
  )
  reset_deprecation_state()
})


# ---- random_portfolios_v1 ----

test_that("random_portfolios_v1() emits deprecation warning", {
  reset_deprecation_state()
  constr <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 0.55,
                  min_sum = 0.99, max_sum = 1.01,
                  weight_seq = generatesequence(min = 0, max = 0.55, by = 0.01))
  )
  # random_portfolios_v1 itself warns, plus randomize_portfolio_v1 warns once
  expect_warning(
    random_portfolios_v1(rpconstraints = constr, permutations = 5),
    "deprecated"
  )
  reset_deprecation_state()
})


# ---- update_constraint_v1tov2 is NOT deprecated (bridge function) ----

test_that("update_constraint_v1tov2() does not emit deprecation warning", {
  constr <- suppressWarnings({
    c1 <- constraint_v1(assets = funds, min = 0, max = 0.55,
                        min_sum = 0.99, max_sum = 1.01)
    add.objective_v1(constraints = c1, type = "return", name = "mean")
  })
  portf <- portfolio.spec(assets = funds)
  expect_silent(
    update_constraint_v1tov2(portfolio = portf, v1_constraint = constr)
  )
})
