##### test_constraints_coverage4.R #####
# Coverage: constraints.R — box_constraint message paths, scalar min_mult/max_mult
# replication, group_constraint scalar group_min/group_max replication,
# add.constraint validation, factor_exposure_constraint validation,
# insert_constraints, and update_constraint_v1tov2 group path.

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]
assets4 <- colnames(R4)

# ===========================================================================
# 1. box_constraint via add.constraint — scalar min/max with message=TRUE
# ===========================================================================

test_that("add.constraint box messages when scalar min/max are replicated", {
  p <- portfolio.spec(assets = assets4)
  expect_message(
    add.constraint(p, type = "box", min = 0.05, max = 0.8, message = TRUE),
    "min not passed in as vector"
  )
})

test_that("add.constraint box defaults to 0/1 when no min/max passed in", {
  p <- portfolio.spec(assets = assets4)
  expect_message(
    p2 <- add.constraint(p, type = "box", message = TRUE),
    "no min or max passed in"
  )
  bc <- p2$constraints[[1]]
  expect_equal(unname(bc$min), rep(0, 4))
  expect_equal(unname(bc$max), rep(1, 4))
})

# ===========================================================================
# 2. box_constraint — min_mult / max_mult paths
# ===========================================================================

test_that("add.constraint box replicates scalar min_mult/max_mult with message", {
  p <- portfolio.spec(assets = assets4)
  # min_mult/max_mult are used to adjust min/max, not stored in constraint
  expect_message(
    p2 <- add.constraint(p, type = "box", min = 0.1, max = 0.5,
                         min_mult = 0.8, max_mult = 1.2, message = TRUE),
    "min_mult and max_mult not passed in as vectors"
  )
})

test_that("add.constraint box rejects mismatched min_mult/max_mult lengths", {
  p <- portfolio.spec(assets = assets4)
  expect_error(
    add.constraint(p, type = "box", min = 0.1, max = 0.5,
                   min_mult = c(0.8, 0.9), max_mult = c(1.1, 1.2, 1.3)),
    "length of min_mult and max_mult must be the same"
  )
})

# ===========================================================================
# 3. group_constraint — scalar group_min/group_max replication
# ===========================================================================

test_that("group_constraint replicates scalar group_min with message", {
  expect_message(
    group_constraint(assets = assets4,
                     groups = list(1:2, 3:4),
                     group_min = 0.1, group_max = 0.9,
                     message = TRUE),
    "group_min not passed in as vector"
  )
})

test_that("group_constraint rejects wrong-length group_min", {
  expect_error(
    group_constraint(assets = assets4,
                     groups = list(1:2, 3:4),
                     group_min = c(0.1, 0.2, 0.3),
                     group_max = c(0.8, 0.9)),
    "length of group_min must be equal to 1 or the length of groups"
  )
})

test_that("group_constraint rejects wrong-length group_max", {
  expect_error(
    group_constraint(assets = assets4,
                     groups = list(1:2, 3:4),
                     group_min = c(0.1, 0.2),
                     group_max = c(0.8, 0.9, 0.7)),
    "length of group_max must be equal to 1 or the length of groups"
  )
})

test_that("group_constraint rejects wrong-length group_labels", {
  expect_error(
    group_constraint(assets = assets4,
                     groups = list(1:2, 3:4),
                     group_min = c(0.1, 0.2),
                     group_max = c(0.8, 0.9),
                     group_labels = c("a", "b", "c")),
    "length of group_labels must be equal to the length of groups"
  )
})

# ===========================================================================
# 4. add.constraint validation
# ===========================================================================

test_that("add.constraint rejects non-portfolio object", {
  expect_error(add.constraint(portfolio = "not_a_portfolio", type = "box"),
               "portfolio passed in is not of class portfolio")
})

# ===========================================================================
# 5. add.constraint — leverage_exposure and filter types
# ===========================================================================

test_that("add.constraint handles leverage_exposure type", {
  p <- portfolio.spec(assets = assets4)
  p <- add.constraint(p, type = "leverage_exposure", leverage = 1.6)
  types <- sapply(p$constraints, function(x) x$type)
  expect_true("leverage_exposure" %in% types)
})

# ===========================================================================
# 6. factor_exposure_constraint validation
# ===========================================================================

test_that("factor_exposure_constraint rejects wrong-length B vector", {
  p <- portfolio.spec(assets = assets4)
  expect_error(
    add.constraint(p, type = "factor_exposure",
                   B = c(1, 2, 3),  # 3 elements, need 4
                   lower = 0.5, upper = 1.5),
    "length of B must be equal to number of assets"
  )
})

test_that("factor_exposure_constraint rejects wrong-length lower for vector B", {
  p <- portfolio.spec(assets = assets4)
  expect_error(
    add.constraint(p, type = "factor_exposure",
                   B = c(1, 2, 3, 4),
                   lower = c(0.5, 0.6),  # need scalar for vector B
                   upper = 1.5),
    "lower must be a scalar"
  )
})

test_that("factor_exposure_constraint rejects wrong-length upper for vector B", {
  p <- portfolio.spec(assets = assets4)
  expect_error(
    add.constraint(p, type = "factor_exposure",
                   B = c(1, 2, 3, 4),
                   lower = 0.5,
                   upper = c(1.5, 1.6)),  # need scalar for vector B
    "upper must be a scalar"
  )
})

test_that("factor_exposure_constraint rejects wrong-nrow B matrix", {
  p <- portfolio.spec(assets = assets4)
  expect_error(
    add.constraint(p, type = "factor_exposure",
                   B = matrix(1:6, nrow = 3, ncol = 2),
                   lower = c(0.5, 0.5), upper = c(1.5, 1.5)),
    "number of rows of B must be equal to number of assets"
  )
})

test_that("factor_exposure_constraint rejects wrong-length lower for matrix B", {
  p <- portfolio.spec(assets = assets4)
  expect_error(
    add.constraint(p, type = "factor_exposure",
                   B = matrix(1:8, nrow = 4, ncol = 2),
                   lower = 0.5,  # need length 2
                   upper = c(1.5, 1.5)),
    "length of lower must be equal to the number of columns in the B matrix"
  )
})

test_that("factor_exposure_constraint rejects wrong-length upper for matrix B", {
  p <- portfolio.spec(assets = assets4)
  expect_error(
    add.constraint(p, type = "factor_exposure",
                   B = matrix(1:8, nrow = 4, ncol = 2),
                   lower = c(0.5, 0.5),
                   upper = 1.5),  # need length 2
    "length of upper must be equal to the number of columns in the B matrix"
  )
})

# ===========================================================================
# 7. insert_constraints validation
# ===========================================================================

test_that("insert_constraints rejects non-list constraints", {
  p <- portfolio.spec(assets = assets4)
  expect_error(insert_constraints(portfolio = p, constraints = "not_a_list"),
               "constraints must be passed in as a list")
})
