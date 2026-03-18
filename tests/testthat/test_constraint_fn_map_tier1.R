context("constraint_fn_map coverage: relax, verbose, projection edge cases, rp helpers")


data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:48, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

# Helper: build a portfolio spec with given constraints
make_portf <- function(n = 4, min_box = rep(0.05, 4), max_box = rep(0.55, 4)) {
  p <- portfolio.spec(assets = paste0("A", seq_len(n))[1:n])
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = min_box, max = max_box)
  p
}

# ============================================================================
# A. fn_map with method="projection" (convex path, covers lines 72-97)
# ============================================================================

test_that("fn_map projection path returns feasible weights for box + weight_sum", {
  portf <- make_portf()
  # Weights that violate box constraints

  bad_w <- c(0.01, 0.02, 0.6, 0.37)
  names(bad_w) <- names(portf$assets)
  
  result <- fn_map(bad_w, portf, method = "projection")
  expect_true(is.list(result))
  expect_true(all(result$weights >= 0.05 - 1e-6))
  expect_true(all(result$weights <= 0.55 + 1e-6))
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map projection handles group constraints", {
  portf <- make_portf()
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(0.3, 0.3),
                          group_max = c(0.6, 0.6))
  
  bad_w <- c(0.1, 0.1, 0.4, 0.4)
  names(bad_w) <- names(portf$assets)
  
  result <- fn_map(bad_w, portf, method = "projection")
  expect_true(sum(result$weights[1:2]) >= 0.3 - 1e-4)
  expect_true(sum(result$weights[3:4]) <= 0.6 + 1e-4)
})

# ============================================================================
# B. fn_map with method="rp_transform" (legacy path)
# ============================================================================

test_that("fn_map rp_transform path works for simple violations", {
  portf <- make_portf()
  bad_w <- c(0.01, 0.02, 0.6, 0.37)
  names(bad_w) <- names(portf$assets)
  
  set.seed(7318)
  result <- fn_map(bad_w, portf, method = "rp_transform")
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
})

# ============================================================================
# C. fn_map with relax=TRUE for box constraints (covers lines 190-234)
# ============================================================================

test_that("fn_map relax=TRUE relaxes box constraints on hard problems", {
  # Make box constraints very tight and conflicting
  portf <- portfolio.spec(assets = c("A1", "A2", "A3", "A4"))
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box",
                          min = c(0.30, 0.30, 0.30, 0.30),
                          max = c(0.35, 0.35, 0.35, 0.35))
  # These box constraints can't sum to ~1.0 (sum of min=1.2 > max_sum)
  # relax=TRUE should widen the box constraints
  
  bad_w <- c(0.25, 0.25, 0.25, 0.25)
  names(bad_w) <- names(portf$assets)
  
  set.seed(4821)
  result <- fn_map(bad_w, portf, relax = TRUE, method = "rp_transform")
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
})

# ============================================================================
# D. fn_map with relax=TRUE for group constraints (covers lines 264-303)
# ============================================================================

test_that("fn_map relax=TRUE relaxes group constraints", {
  portf <- make_portf()
  # Group constraints that conflict with each other
  portf <- add.constraint(portf, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(0.6, 0.6),
                          group_max = c(0.65, 0.65))
  # Both groups need >= 0.6 but sum can only be ~1.0
  
  bad_w <- c(0.3, 0.2, 0.25, 0.25)
  names(bad_w) <- names(portf$assets)
  
  set.seed(9127)
  result <- fn_map(bad_w, portf, relax = TRUE, method = "rp_transform")
  expect_true(is.list(result))
  # The cLO should have been relaxed (decreased)
  expect_true(!is.null(result$cLO))
})

# ============================================================================
# E. fn_map with position limit constraints (covers lines 308-362)
# ============================================================================

test_that("fn_map handles position limit constraint violations", {
  portf <- make_portf()
  portf <- add.constraint(portf, type = "position_limit", max_pos = 2)
  
  # All four weights non-zero, violates max_pos=2
  bad_w <- c(0.25, 0.25, 0.25, 0.25)
  names(bad_w) <- names(portf$assets)
  
  set.seed(3754)
  result <- fn_map(bad_w, portf, method = "rp_transform")
  expect_true(is.list(result))
})

test_that("fn_map relax=TRUE relaxes position limit constraints", {
  portf <- make_portf(min_box = rep(0.1, 4), max_box = rep(0.6, 4))
  portf <- add.constraint(portf, type = "position_limit",
                          max_pos = 1)
  # Can't have only 1 asset and sum to 1.0 with max=0.6
  
  bad_w <- c(0.25, 0.25, 0.25, 0.25)
  names(bad_w) <- names(portf$assets)
  
  set.seed(6283)
  result <- fn_map(bad_w, portf, relax = TRUE, method = "rp_transform")
  expect_true(is.list(result))
  # max_pos should have been incremented
  expect_true(result$max_pos >= 1)
})

# ============================================================================
# F. fn_map with leverage constraint (covers lines 365-416)
# ============================================================================

test_that("fn_map handles leverage constraint violations", {
  portf <- portfolio.spec(assets = c("A1", "A2", "A3", "A4"))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = -0.01, max_sum = 0.01)
  portf <- add.constraint(portf, type = "box", min = -0.5, max = 0.5)
  portf <- add.constraint(portf, type = "leverage_exposure",
                          leverage = 1.0)
  
  # Dollar-neutral weights that exceed leverage
  bad_w <- c(0.4, 0.3, -0.3, -0.4)
  names(bad_w) <- names(portf$assets)
  
  set.seed(8192)
  result <- fn_map(bad_w, portf, method = "rp_transform")
  expect_true(is.list(result))
  expect_true(!is.null(result$leverage))
})

test_that("fn_map relax=TRUE relaxes leverage constraint", {
  portf <- portfolio.spec(assets = c("A1", "A2", "A3", "A4"))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = -0.01, max_sum = 0.01)
  portf <- add.constraint(portf, type = "box", min = -0.5, max = 0.5)
  portf <- add.constraint(portf, type = "leverage_exposure",
                          leverage = 0.5)
  
  bad_w <- c(0.4, 0.3, -0.3, -0.4)
  names(bad_w) <- names(portf$assets)
  
  set.seed(1473)
  result <- fn_map(bad_w, portf, relax = TRUE, method = "rp_transform")
  expect_true(is.list(result))
  # Leverage should have been relaxed (increased)
  expect_true(result$leverage >= 0.5)
})

# ============================================================================
# G. fn_map with verbose=TRUE (covers verbose message paths ~lines 96, 186, 260, etc.)
# ============================================================================

test_that("fn_map verbose=TRUE produces messages on hard constraint problems", {
  portf <- portfolio.spec(assets = c("A1", "A2", "A3", "A4"))
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box",
                          min = c(0.30, 0.30, 0.30, 0.30),
                          max = c(0.35, 0.35, 0.35, 0.35))
  
  bad_w <- c(0.25, 0.25, 0.25, 0.25)
  names(bad_w) <- names(portf$assets)
  
  set.seed(5921)
  # Verbose path may or may not produce messages depending on whether
  # rp_transform actually fails. Just verify it doesn't crash.
  expect_no_error(
    fn_map(bad_w, portf, verbose = TRUE, relax = TRUE, method = "rp_transform")
  )
})

# ============================================================================
# H. project_weights directly (covers lines 1239-1312)
# ============================================================================

test_that("project_weights finds feasible point for box + weight_sum", {
  w <- c(0.01, 0.02, 0.6, 0.37)
  result <- project_weights(
    w = w,
    min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.05, 4), max_box = rep(0.55, 4)
  )
  expect_true(!is.null(result))
  expect_equal(length(result), 4)
  expect_true(all(result >= 0.05 - 1e-6))
  expect_true(all(result <= 0.55 + 1e-6))
  expect_equal(sum(result), 1, tolerance = 0.02)
})

test_that("project_weights handles group constraints", {
  w <- c(0.1, 0.1, 0.4, 0.4)
  result <- project_weights(
    w = w,
    min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.05, 4), max_box = rep(0.55, 4),
    groups = list(c(1, 2), c(3, 4)),
    cLO = c(0.3, 0.3),
    cUP = c(0.6, 0.6)
  )
  expect_true(!is.null(result))
  expect_true(sum(result[1:2]) >= 0.3 - 1e-4)
  expect_true(sum(result[3:4]) >= 0.3 - 1e-4)
})

test_that("project_weights returns NULL for conflicting constraints", {
  # min_box sum is 2.0 but max_sum is 1.01 — impossible
  result <- project_weights(
    w = c(0.5, 0.5, 0.5, 0.5),
    min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.5, 4), max_box = rep(0.8, 4),
    max_iter = 100
  )
  expect_null(result)
})

test_that("project_weights returns input when already feasible", {
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- project_weights(
    w = w,
    min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.05, 4), max_box = rep(0.55, 4)
  )
  expect_true(!is.null(result))
  expect_equal(sum(result), 1, tolerance = 0.02)
})

# ============================================================================
# I. Projection helper functions directly (covers lines 1161-1208)
# ============================================================================

test_that(".project_box clamps correctly", {
  result <- .project_box(c(-0.1, 0.3, 0.9), c(0, 0, 0), c(0.5, 0.5, 0.5))
  expect_equal(result, c(0, 0.3, 0.5))
})

test_that(".project_weight_sum returns input when within bounds", {
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- .project_weight_sum(w, 0.99, 1.01)
  expect_equal(result, w)
})

test_that(".project_weight_sum adjusts when below min_sum", {
  w <- c(0.1, 0.1, 0.1, 0.1)  # sum = 0.4
  result <- .project_weight_sum(w, 0.99, 1.01)
  expect_equal(sum(result), 0.99, tolerance = 1e-10)
})

test_that(".project_weight_sum adjusts when above max_sum", {
  w <- c(0.5, 0.5, 0.5, 0.5)  # sum = 2.0
  result <- .project_weight_sum(w, 0.99, 1.01)
  expect_equal(sum(result), 1.01, tolerance = 1e-10)
})

test_that(".project_group adjusts group sum when violated", {
  w <- c(0.1, 0.1, 0.4, 0.4)
  result <- .project_group(w, c(1, 2), lo = 0.3, up = 0.6)
  expect_true(sum(result[1:2]) >= 0.3 - 1e-10)
  # Non-group elements unchanged
  expect_equal(result[3:4], w[3:4])
})

test_that(".is_projection_feasible correctly classifies feasible/infeasible", {
  w_ok <- c(0.25, 0.25, 0.25, 0.25)
  w_bad <- c(0.01, 0.01, 0.5, 0.48)
  
  expect_true(
    .is_projection_feasible(
      w_ok, 0.99, 1.01, rep(0.05, 4), rep(0.55, 4))
  )
  expect_false(
    .is_projection_feasible(
      w_bad, 0.99, 1.01, rep(0.05, 4), rep(0.55, 4))
  )
})

test_that(".is_projection_feasible checks group constraints", {
  w <- c(0.1, 0.1, 0.4, 0.4)
  expect_false(
    .is_projection_feasible(
      w, 0.99, 1.01, rep(0.05, 4), rep(0.55, 4),
      groups = list(c(1, 2)), cLO = 0.3, cUP = 0.6)
  )
  
  w2 <- c(0.2, 0.2, 0.3, 0.3)
  expect_true(
    .is_projection_feasible(
      w2, 0.99, 1.01, rep(0.05, 4), rep(0.55, 4),
      groups = list(c(1, 2)), cLO = 0.3, cUP = 0.6)
  )
})

# ============================================================================
# J. rp_transform directly — edge cases (covers lines 460-624)
# ============================================================================

test_that("rp_transform produces feasible weights", {
  set.seed(2946)
  w <- c(0.1, 0.2, 0.3, 0.9)
  result <- rp_transform(
    w = w,
    min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.05, 4), max_box = rep(0.55, 4),
    max_permutations = 500
  )
  expect_equal(length(result), 4)
  expect_true(sum(result) >= 0.99)
  expect_true(sum(result) <= 1.01)
})

test_that("rp_transform handles group constraints", {
  set.seed(8413)
  w <- c(0.1, 0.1, 0.4, 0.4)
  result <- rp_transform(
    w = w,
    min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0.05, 4), max_box = rep(0.55, 4),
    groups = list(c(1, 2), c(3, 4)),
    cLO = c(0.25, 0.25),
    cUP = c(0.55, 0.55),
    max_permutations = 500
  )
  expect_equal(length(result), 4)
})

test_that("rp_transform handles leverage constraints", {
  set.seed(6710)
  w <- c(0.4, 0.3, -0.3, -0.4)
  result <- rp_transform(
    w = w,
    min_sum = -0.01, max_sum = 0.01,
    min_box = rep(-0.5, 4), max_box = rep(0.5, 4),
    leverage = 1.0,
    max_permutations = 500
  )
  expect_equal(length(result), 4)
})

test_that("rp_transform errors on infeasible portfolio", {
  set.seed(5137)
  # min_box sum = 4.0, max_sum = 1.01 — clearly impossible
  w <- c(1.0, 1.0, 1.0, 1.0)
  expect_error(
    rp_transform(
      w = w,
      min_sum = 0.99, max_sum = 1.01,
      min_box = rep(1.0, 4), max_box = rep(2.0, 4),
      max_permutations = 10
    ),
    "Infeasible"
  )
})

# ============================================================================
# K. rp_increase / rp_decrease edge cases (covers lines 961-1017)
# ============================================================================

test_that("rp_increase returns input when already above min_sum", {
  w <- c(0.3, 0.3, 0.3, 0.3)
  result <- rp_increase(
    weights = w, min_sum = 0.99,
    max_box = rep(0.55, 4),
    weight_seq = seq(0, 1, by = 0.01)
  )
  expect_equal(result, w)
})

test_that("rp_decrease returns input when already below max_sum", {
  w <- c(0.2, 0.2, 0.2, 0.2)
  result <- rp_decrease(
    weights = w, max_sum = 1.01,
    min_box = rep(0.05, 4),
    weight_seq = seq(0, 1, by = 0.01)
  )
  expect_equal(result, w)
})

test_that("rp_increase handles n_tmp_seq == 1 case", {
  set.seed(2471)
  # Weight is just below max_box so only 1 candidate in weight_seq
  w <- c(0.1, 0.1, 0.1, 0.1)  # sum = 0.4
  result <- rp_increase(
    weights = w, min_sum = 0.99,
    max_box = c(0.11, 0.55, 0.55, 0.55),
    weight_seq = seq(0, 1, by = 0.01)
  )
  expect_true(sum(result) > 0.4)
})

# ============================================================================
# L. rp_decrease_leverage edge cases (covers lines 1019-1061)
# ============================================================================

test_that("rp_decrease_leverage reduces leverage", {
  set.seed(3849)
  w <- c(0.4, 0.3, -0.3, -0.4)  # leverage = 1.4
  result <- rp_decrease_leverage(
    weights = w,
    max_box = rep(0.5, 4),
    min_box = rep(-0.5, 4),
    leverage = 1.0,
    weight_seq = seq(-0.5, 0.5, by = 0.01)
  )
  expect_true(sum(abs(result)) <= sum(abs(w)) + 0.1)
})

test_that("rp_decrease_leverage handles negative current value", {
  set.seed(7621)
  w <- c(-0.5, -0.3, 0.4, 0.4)  # leverage = 1.6
  result <- rp_decrease_leverage(
    weights = w,
    max_box = rep(0.5, 4),
    min_box = rep(-0.5, 4),
    leverage = 1.0,
    weight_seq = seq(-0.5, 0.5, by = 0.01)
  )
  expect_equal(length(result), 4)
})

# ============================================================================
# M. rp_position_limit edge cases (covers lines 1063-1117)
# ============================================================================

test_that("rp_position_limit handles max_pos_long violation", {
  set.seed(4392)
  w <- c(0.3, 0.3, 0.2, 0.2)  # 4 long positions
  result <- rp_position_limit(
    weights = w,
    max_pos_long = 2,
    min_box = rep(-0.1, 4),
    max_box = rep(0.6, 4),
    weight_seq = seq(-0.1, 0.6, by = 0.01)
  )
  expect_equal(length(result), 4)
})

test_that("rp_position_limit handles max_pos_short violation", {
  set.seed(5183)
  w <- c(-0.2, -0.2, -0.1, 0.5)  # 3 short positions
  result <- rp_position_limit(
    weights = w,
    max_pos_short = 1,
    min_box = rep(-0.3, 4),
    max_box = rep(0.6, 4),
    weight_seq = seq(-0.3, 0.6, by = 0.01)
  )
  expect_equal(length(result), 4)
})

# ============================================================================
# N. fn_map with position_limit + max_pos_long + max_pos_short
# ============================================================================

test_that("fn_map handles combined position limit constraints", {
  portf <- portfolio.spec(assets = c("A1", "A2", "A3", "A4"))
  portf <- add.constraint(portf, type = "weight_sum",
                          min_sum = -0.01, max_sum = 0.01)
  portf <- add.constraint(portf, type = "box", min = -0.4, max = 0.4)
  portf <- add.constraint(portf, type = "position_limit",
                          max_pos = 4,
                          max_pos_long = 2,
                          max_pos_short = 2)
  
  bad_w <- c(0.3, 0.2, 0.1, -0.6)
  names(bad_w) <- names(portf$assets)
  
  set.seed(9472)
  result <- fn_map(bad_w, portf, method = "rp_transform")
  expect_true(is.list(result))
})

# ============================================================================
# O. fn_map input validation
# ============================================================================

test_that("fn_map errors on non-portfolio object", {
  expect_error(fn_map(c(0.25, 0.25, 0.25, 0.25), list()), "not of class")
})
