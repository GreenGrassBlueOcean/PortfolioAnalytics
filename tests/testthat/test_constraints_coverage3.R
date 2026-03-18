##### test_constraints_coverage3.R #####
# Coverage: constraints.R — box_constraint scalar/vector, group_constraint optionals

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]

# ===========================================================================
# 1. box_constraint with scalar min/max
# ===========================================================================

test_that("box_constraint with scalar min and max", {
  p <- portfolio.spec(assets = colnames(R3))
  p <- add.constraint(p, type = "box", min = 0.1, max = 0.5)
  cstr <- get_constraints(p)
  expect_equal(length(cstr$min), 3)
  expect_equal(length(cstr$max), 3)
})

# ===========================================================================
# 2. box_constraint with vector min/max
# ===========================================================================

test_that("box_constraint with vector min and max", {
  p <- portfolio.spec(assets = colnames(R3))
  p <- add.constraint(p, type = "box",
                      min = c(0.1, 0.2, 0.3),
                      max = c(0.5, 0.6, 0.7))
  cstr <- get_constraints(p)
  expect_equal(unname(cstr$min), c(0.1, 0.2, 0.3))
  expect_equal(unname(cstr$max), c(0.5, 0.6, 0.7))
})

# ===========================================================================
# 3. group_constraint with min_mult/max_mult
# ===========================================================================

test_that("group_constraint with min_mult and max_mult", {
  p <- portfolio.spec(assets = colnames(R3))
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3),
                      group_min = c(0.3, 0.1),
                      group_max = c(0.7, 0.5),
                      group_pos = c(2, 1))
  cstr <- get_constraints(p)
  expect_equal(length(cstr$groups), 2)
  expect_equal(cstr$cLO, c(0.3, 0.1))
  expect_equal(cstr$cUP, c(0.7, 0.5))
})
