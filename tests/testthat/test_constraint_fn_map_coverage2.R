##### test_constraint_fn_map_coverage2.R #####
# Coverage tests for constraint_fn_map.R: relax paths, group violations,
# verbose messaging, position_limit relax, leverage relax

# ===========================================================================
# 1. fn_map relax=TRUE: box constraint try-error → relaxation loop
# ===========================================================================

test_that("fn_map relax=TRUE exercises box constraint relaxation loop", {
  # sum(max_box) = 0.45 < min_sum = 0.99 → rp_transform must fail
  # Weights violate box constraints AND weight_sum
  p <- portfolio.spec(assets = 3)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box",
                      min = c(0.1, 0.1, 0.1),
                      max = c(0.15, 0.15, 0.15))

  w <- c(0.05, 0.05, 0.05)
  set.seed(4217)
  result <- fn_map(weights = w, portfolio = p, relax = TRUE,
                   verbose = FALSE, method = "rp_transform")
  expect_equal(length(result$weights), 3)
  # Relaxation couldn't fix it → falls back to original, resets min/max
  expect_equal(unname(result$min), c(0.1, 0.1, 0.1))
  expect_equal(unname(result$max), c(0.15, 0.15, 0.15))
})

# ===========================================================================
# 2. fn_map relax=TRUE: box constraint with verbose messaging
# ===========================================================================

test_that("fn_map verbose=TRUE prints error message on box constraint failure", {
  p <- portfolio.spec(assets = 3)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box",
                      min = c(0.1, 0.1, 0.1),
                      max = c(0.15, 0.15, 0.15))
  w <- c(0.05, 0.05, 0.05)
  set.seed(4218)
  expect_message(
    fn_map(weights = w, portfolio = p, relax = TRUE,
           verbose = TRUE, method = "rp_transform"),
    "Infeasible"
  )
})

# ===========================================================================
# 3. fn_map relax=TRUE: group constraint try-error → relaxation loop
# ===========================================================================

test_that("fn_map relax=TRUE exercises group constraint relaxation loop", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  # Both groups require 0.8–0.9, but sum would be 1.6–1.8 → infeasible
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.8, 0.8),
                      group_max = c(0.9, 0.9))

  w <- c(0.3, 0.3, 0.2, 0.2)
  set.seed(7821)
  result <- fn_map(weights = w, portfolio = p, relax = TRUE,
                   verbose = FALSE, method = "rp_transform")
  expect_equal(length(result$weights), 4)
  # cLO/cUP should be reset to original since relaxation failed
  expect_equal(result$cLO, c(0.8, 0.8))
  expect_equal(result$cUP, c(0.9, 0.9))
})

# ===========================================================================
# 4. fn_map verbose=TRUE on group constraint failure
# ===========================================================================

test_that("fn_map verbose=TRUE prints message on group constraint failure", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.65)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.8, 0.8),
                      group_max = c(0.9, 0.9))

  w <- c(0.3, 0.3, 0.2, 0.2)
  set.seed(7822)
  expect_message(
    fn_map(weights = w, portfolio = p, relax = TRUE,
           verbose = TRUE, method = "rp_transform"),
    "Infeasible"
  )
})

# ===========================================================================
# 5. fn_map relax=FALSE: box constraint try-error without relaxation
# ===========================================================================

test_that("fn_map relax=FALSE returns original weights when box transform fails", {
  p <- portfolio.spec(assets = 3)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box",
                      min = c(0.1, 0.1, 0.1),
                      max = c(0.15, 0.15, 0.15))

  w <- c(0.05, 0.05, 0.05)
  set.seed(4219)
  result <- fn_map(weights = w, portfolio = p, relax = FALSE,
                   method = "rp_transform")
  expect_equal(length(result$weights), 3)
})

# ===========================================================================
# 6. fn_map with non-portfolio object errors
# ===========================================================================

test_that("fn_map errors when portfolio is not of class portfolio", {
  expect_error(fn_map(weights = c(0.5, 0.5), portfolio = list()),
               "portfolio passed in is not of class")
})

# ===========================================================================
# 7. fn_map with position_limit relax path
# ===========================================================================

test_that("fn_map relax=TRUE exercises position_limit relaxation", {
  # 4 assets, max_pos=1 but all min_box > 0 → impossible to have only 1 position
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.1, max = 0.5)
  p <- add.constraint(p, type = "position_limit", max_pos = 1)

  w <- c(0.25, 0.25, 0.25, 0.25)
  set.seed(3319)
  result <- fn_map(weights = w, portfolio = p, relax = TRUE,
                   verbose = FALSE, method = "rp_transform")
  expect_equal(length(result$weights), 4)
})

# ===========================================================================
# 8. fn_map with leverage constraint violation + relax
# ===========================================================================

test_that("fn_map handles leverage constraint violation with relax", {
  p <- portfolio.spec(assets = 3)
  p <- add.constraint(p, type = "weight_sum", min_sum = -0.5, max_sum = 0.5)
  p <- add.constraint(p, type = "box", min = -0.8, max = 0.8)
  p <- add.constraint(p, type = "leverage_exposure", leverage = 0.5)

  w <- c(0.8, 0.5, -0.8)
  set.seed(5501)
  result <- fn_map(weights = w, portfolio = p, relax = TRUE,
                   verbose = FALSE, method = "rp_transform")
  expect_equal(length(result$weights), 3)
})

# ===========================================================================
# 9. fn_map projection skipped for non-convex constraints
# ===========================================================================

test_that("fn_map projection path skipped when constraints are non-convex", {
  p <- portfolio.spec(assets = 3)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.70)
  p <- add.constraint(p, type = "position_limit", max_pos = 2)

  w <- c(0.5, 0.3, 0.2)
  result <- fn_map(weights = w, portfolio = p, method = "projection")
  expect_equal(length(result$weights), 3)
})
