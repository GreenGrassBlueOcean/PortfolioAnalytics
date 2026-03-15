library(testthat)
library(PortfolioAnalytics)

context("constraints advanced: constructors, helpers, edge cases")

# ============================================================================
# Shared setup
# ============================================================================
data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:36, 1:4]
funds4 <- colnames(R4)

# ============================================================================
# A. add.constraint dispatch: null type returns portfolio unchanged
# ============================================================================

test_that("add.constraint type='null' returns portfolio unchanged", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  n_before <- length(spec$constraints)
  spec2 <- add.constraint(spec, type = "null")
  expect_equal(length(spec2$constraints), n_before)
})

# ============================================================================
# B. transaction_cost_constraint
# ============================================================================

test_that("transaction_cost_constraint scalar expansion", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "transaction_cost", ptc = 0.01)
  constr <- get_constraints(spec)
  expect_equal(length(constr$ptc), 4)
  expect_equal(constr$ptc, rep(0.01, 4))
})

test_that("transaction_cost_constraint per-asset vector", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  ptc_vec <- c(0.01, 0.02, 0.005, 0.015)
  spec <- add.constraint(spec, type = "transaction_cost", ptc = ptc_vec)
  constr <- get_constraints(spec)
  expect_equal(constr$ptc, ptc_vec)
})

test_that("transaction_cost_constraint errors on wrong length", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  expect_error(
    add.constraint(spec, type = "transaction_cost", ptc = c(0.01, 0.02)),
    "length of ptc"
  )
})

test_that("add.constraint accepts 'transaction' as alias", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "transaction", ptc = 0.01)
  constr <- get_constraints(spec)
  expect_equal(length(constr$ptc), 4)
})

# ============================================================================
# C. box_constraint: min_mult / max_mult
# ============================================================================

test_that("box_constraint with min_mult/max_mult adjusts bounds", {
  assets <- c(A = 0.3, B = 0.3, C = 0.2, D = 0.2)
  spec <- portfolio.spec(assets = assets)
  spec <- add.constraint(spec, type = "full_investment")
  # min_mult=0.8 means min_i >= assets_i * 0.8 where that exceeds default min
  # max_mult=1.2 means max_i <= assets_i * 1.2 where that is less than default max
  spec <- add.constraint(spec, type = "box", min = 0, max = 1,
                         min_mult = 0.8, max_mult = 1.2)
  constr <- get_constraints(spec)
  # For asset A: assets*min_mult = 0.3*0.8 = 0.24 > min=0, so min becomes 0.24
  expect_equal(as.numeric(constr$min["A"]), 0.24)
  # For asset A: assets*max_mult = 0.3*1.2 = 0.36 < max=1, so max becomes 0.36
  expect_equal(as.numeric(constr$max["A"]), 0.36)
})

test_that("box_constraint long_only sets min=0 max=1", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "long_only")
  constr <- get_constraints(spec)
  expect_equal(as.numeric(constr$min), rep(0, 4))
  expect_equal(as.numeric(constr$max), rep(1, 4))
})

test_that("box_constraint errors on mismatched min/max lengths", {
  spec <- portfolio.spec(assets = funds4)
  expect_error(
    add.constraint(spec, type = "box", min = c(0, 0, 0), max = c(1, 1)),
    "length of min and max"
  )
})

test_that("box_constraint errors on wrong vector length", {
  spec <- portfolio.spec(assets = funds4)
  expect_error(
    add.constraint(spec, type = "box", min = c(0, 0, 0), max = c(1, 1, 1)),
    "length of min must"
  )
})

# ============================================================================
# D. group_constraint: group_pos validation
# ============================================================================

test_that("group_constraint with group_pos", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "group",
                         groups = list(g1 = c(1, 2), g2 = c(3, 4)),
                         group_min = c(0.2, 0.2),
                         group_max = c(0.8, 0.8),
                         group_pos = c(1, 2))
  constr <- get_constraints(spec)
  expect_equal(constr$group_pos, c(1, 2))
})

test_that("group_constraint group_pos capped at group size", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  # group_pos = 5 but group has only 2 assets; should be capped
  spec <- add.constraint(spec, type = "group",
                         groups = list(g1 = c(1, 2), g2 = c(3, 4)),
                         group_min = c(0.2, 0.2),
                         group_max = c(0.8, 0.8),
                         group_pos = c(5, 5))
  constr <- get_constraints(spec)
  expect_equal(constr$group_pos, c(2, 2))
})

test_that("group_constraint errors on wrong group_pos length", {
  spec <- portfolio.spec(assets = funds4)
  expect_error(
    add.constraint(spec, type = "group",
                   groups = list(1:2, 3:4),
                   group_min = c(0.2, 0.2),
                   group_max = c(0.8, 0.8),
                   group_pos = c(1, 1, 1)),
    "length of group_pos"
  )
})

test_that("group_constraint errors on negative group_pos", {
  spec <- portfolio.spec(assets = funds4)
  expect_error(
    add.constraint(spec, type = "group",
                   groups = list(1:2, 3:4),
                   group_min = c(0.2, 0.2),
                   group_max = c(0.8, 0.8),
                   group_pos = c(-1, 1)),
    "positive"
  )
})

test_that("group_constraint errors on non-list groups", {
  spec <- portfolio.spec(assets = funds4)
  expect_error(
    add.constraint(spec, type = "group",
                   groups = c(1, 2, 3, 4),
                   group_min = 0.2,
                   group_max = 0.8),
    "groups must be passed in as a list"
  )
})

test_that("group_constraint uses named list for labels", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "group",
                         groups = list(equity = c(1, 2), bond = c(3, 4)),
                         group_min = c(0.3, 0.3),
                         group_max = c(0.7, 0.7))
  constr <- get_constraints(spec)
  expect_equal(constr$group_labels, c("equity", "bond"))
})

test_that("group_constraint scalar group_min/group_max expansion", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "group",
                         groups = list(1:2, 3:4),
                         group_min = 0.2,
                         group_max = 0.8)
  constr <- get_constraints(spec)
  expect_equal(constr$cLO, c(0.2, 0.2))
  expect_equal(constr$cUP, c(0.8, 0.8))
})

# ============================================================================
# E. weight_sum_constraint aliases
# ============================================================================

test_that("weight_sum_constraint full_investment sets min_sum=max_sum=1", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  constr <- get_constraints(spec)
  expect_equal(constr$min_sum, 1)
  expect_equal(constr$max_sum, 1)
})

test_that("weight_sum_constraint dollar_neutral sets min_sum=max_sum=0", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "dollar_neutral")
  constr <- get_constraints(spec)
  expect_equal(constr$min_sum, 0)
  expect_equal(constr$max_sum, 0)
})

test_that("weight_sum_constraint active alias works", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "active")
  constr <- get_constraints(spec)
  expect_equal(constr$min_sum, 0)
  expect_equal(constr$max_sum, 0)
})

test_that("weight_sum_constraint leverage alias works", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "leverage", min_sum = 0.95, max_sum = 1.05)
  constr <- get_constraints(spec)
  expect_equal(constr$min_sum, 0.95)
  expect_equal(constr$max_sum, 1.05)
})

# ============================================================================
# F. leverage_exposure_constraint
# ============================================================================

test_that("leverage_exposure_constraint stores leverage value", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = -0.5, max = 0.5)
  spec <- add.constraint(spec, type = "leverage_exposure", leverage = 1.6)
  constr <- get_constraints(spec)
  expect_equal(constr$leverage, 1.6)
})

# ============================================================================
# G. get_constraints defaults when constraints are missing
# ============================================================================

test_that("get_constraints defaults to full_investment when no weight_sum", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)
  constr <- get_constraints(spec)
  expect_equal(constr$min_sum, 1)
  expect_equal(constr$max_sum, 1)
})

test_that("get_constraints defaults to unconstrained box when no box", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  constr <- get_constraints(spec)
  expect_equal(as.numeric(constr$min), rep(-Inf, 4))
  expect_equal(as.numeric(constr$max), rep(Inf, 4))
})

test_that("get_constraints errors on empty constraints", {
  spec <- portfolio.spec(assets = funds4)
  expect_error(get_constraints(spec), "No constraints")
})

test_that("get_constraints skips disabled constraints", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.4, enabled = FALSE)
  constr <- get_constraints(spec)
  # Box was disabled, so defaults to -Inf/Inf
  expect_equal(as.numeric(constr$min), rep(-Inf, 4))
})

# ============================================================================
# H. indexnum parameter for updating constraints
# ============================================================================

test_that("add.constraint indexnum updates existing constraint", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  spec <- add.constraint(spec, type = "box", min = 0, max = 1)

  # Update the box constraint (indexnum=2) to tighter bounds
  spec <- add.constraint(spec, type = "box", min = 0.1, max = 0.5, indexnum = 2)
  expect_length(spec$constraints, 2)
  constr <- get_constraints(spec)
  expect_equal(as.numeric(constr$min), rep(0.1, 4))
  expect_equal(as.numeric(constr$max), rep(0.5, 4))
})

# ============================================================================
# I. is.constraint
# ============================================================================

test_that("is.constraint correctly identifies constraint objects", {
  c1 <- constraint_v2(type = "box", enabled = TRUE)
  expect_true(is.constraint(c1))
  expect_false(is.constraint(list(type = "box")))
  expect_false(is.constraint("not a constraint"))
})

# ============================================================================
# J. factor_exposure_constraint
# ============================================================================

test_that("factor_exposure_constraint with vector B", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  B <- c(0.8, 1.0, 1.2, 0.9)
  spec <- add.constraint(spec, type = "factor_exposure",
                         B = B, lower = 0.9, upper = 1.1)
  constr <- get_constraints(spec)
  expect_true(is.matrix(constr$B))
  expect_equal(nrow(constr$B), 4)
  expect_equal(ncol(constr$B), 1)
})

test_that("factor_exposure_constraint with matrix B", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  B <- matrix(runif(8), nrow = 4, ncol = 2)
  spec <- add.constraint(spec, type = "factor_exposure",
                         B = B, lower = c(0.5, 0.5), upper = c(1.5, 1.5))
  constr <- get_constraints(spec)
  expect_equal(nrow(constr$B), 4)
  expect_equal(ncol(constr$B), 2)
  expect_true(!is.null(colnames(constr$B)))
})

test_that("factor_exposure_constraint errors on dimension mismatch", {
  spec <- portfolio.spec(assets = funds4)
  expect_error(
    add.constraint(spec, type = "factor_exposure",
                   B = c(1, 2, 3), lower = 0.5, upper = 1.5),
    "length of B"
  )
})

test_that("factor_exposures alias works", {
  spec <- portfolio.spec(assets = funds4)
  spec <- add.constraint(spec, type = "full_investment")
  B <- c(1, 1, 1, 1)
  spec <- add.constraint(spec, type = "factor_exposures",
                         B = B, lower = 0.9, upper = 1.1)
  constr <- get_constraints(spec)
  expect_true(!is.null(constr$B))
})

# ============================================================================
# K. insert_constraints helper
# ============================================================================

test_that("insert_constraints replaces constraints list", {
  spec <- portfolio.spec(assets = funds4)
  c1 <- weight_sum_constraint(min_sum = 1, max_sum = 1)
  c2 <- box_constraint(assets = spec$assets, min = 0, max = 1)
  spec2 <- PortfolioAnalytics:::insert_constraints(spec, list(c1, c2))
  expect_length(spec2$constraints, 2)
})

test_that("insert_constraints errors on non-constraint objects", {
  spec <- portfolio.spec(assets = funds4)
  expect_error(
    PortfolioAnalytics:::insert_constraints(spec, list(list(type = "box"))),
    "class 'constraint'"
  )
})

test_that("insert_constraints errors on non-portfolio", {
  expect_error(
    PortfolioAnalytics:::insert_constraints(list(), list()),
    "class portfolio"
  )
})

# ============================================================================
# L. constraint_v1 (deprecated)
# ============================================================================

test_that("constraint_v1 issues deprecation warning", {
  lifecycle::expect_deprecated(
    result <- constraint_v1(assets = 4, min = 0, max = 1)
  )
})

# ============================================================================
# M. constraint_v2 basic constructor
# ============================================================================

test_that("constraint_v2 creates v2_constraint object", {
  c1 <- constraint_v2(type = "test_type", enabled = TRUE)
  expect_true(inherits(c1, "v2_constraint"))
  expect_true(inherits(c1, "constraint"))
  expect_equal(c1$type, "test_type")
  expect_true(c1$enabled)
})

test_that("constraint() without type or assets errors", {
  expect_error(constraint(), "must specify a constraint type")
})
