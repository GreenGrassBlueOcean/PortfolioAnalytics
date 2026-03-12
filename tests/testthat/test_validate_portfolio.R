library(testthat)
library(PortfolioAnalytics)
library(xts)

context("Proposal #11: validate_portfolio()")


# ---- Helpers ----

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[, 1:4]
funds <- colnames(R4)

make_good_portf <- function() {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
}


# ---- E1: Not a portfolio object ----

test_that("E1: non-portfolio object triggers immediate stop", {
  expect_error(validate_portfolio("not_a_portfolio"), "E1")
  expect_error(validate_portfolio(list(assets = 1:4)), "E1")
})


# ---- E2: No assets ----

test_that("E2: portfolio with no assets triggers error", {
  p <- make_good_portf()
  p$assets <- NULL
  expect_error(validate_portfolio(p), "E2")
})


# ---- E3: Duplicate asset names ----

test_that("E3: duplicate asset names detected", {
  p <- make_good_portf()
  names(p$assets) <- c("A", "A", "B", "C")
  expect_error(validate_portfolio(p), "E3")
})


# ---- E4: NA / empty asset names ----

test_that("E4: NA asset name detected", {
  p <- make_good_portf()
  names(p$assets)[2] <- NA
  expect_error(validate_portfolio(p), "E4")
})

test_that("E4: empty string asset name detected", {
  p <- make_good_portf()
  names(p$assets)[3] <- ""
  expect_error(validate_portfolio(p), "E4")
})


# ---- E5: R has fewer columns than assets ----

test_that("E5: R with too few columns triggers error", {
  p <- make_good_portf()
  expect_error(validate_portfolio(p, R = R4[, 1:2]), "E5")
})


# ---- E6: Asset names not in R columns ----

test_that("E6: portfolio assets missing from R columns", {
  p <- make_good_portf()
  R_wrong <- R4
  colnames(R_wrong) <- c("X1", "X2", "X3", "X4")
  expect_error(validate_portfolio(p, R = R_wrong), "E6")
})


# ---- E7: R has fewer than 2 rows ----

test_that("E7: single-row R triggers error", {
  p <- make_good_portf()
  expect_error(validate_portfolio(p, R = R4[1, ]), "E7")
})


# ---- E8: Column entirely NA ----

test_that("E8: all-NA column detected", {
  p <- make_good_portf()
  R_na <- R4
  R_na[, 2] <- NA
  expect_error(validate_portfolio(p, R = R_na), "E8")
})


# ---- E9: Box min > max ----

test_that("E9: box constraint with min > max detected", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = c(0.5, 0, 0, 0),
                      max = c(0.3, 1, 1, 1))
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_error(validate_portfolio(p), "E9")
})


# ---- E10: Weight sum min_sum > max_sum ----

test_that("E10: min_sum > max_sum detected", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 1.5, max_sum = 0.5)
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_error(validate_portfolio(p), "E10")
})


# ---- E11: Group min > max ----

test_that("E11: group constraint with min > max detected", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "group",
                      groups = list(first = c(1, 2), second = c(3, 4)),
                      group_min = c(0.6, 0.7),
                      group_max = c(0.4, 0.3))
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_error(validate_portfolio(p), "E11")
})


# ---- E12: Factor exposure lower > upper ----

test_that("E12: factor exposure lower > upper detected", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  set.seed(4721)
  B <- matrix(rnorm(8), ncol = 2)
  rownames(B) <- funds
  p <- add.constraint(p, type = "factor_exposure", B = B,
                      lower = c(0.5, 0.5), upper = c(0.3, 0.3))
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_error(validate_portfolio(p), "E12")
})


# ---- E13: Obvious infeasibility ----

test_that("E13: sum(box_max) < min_sum detected", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 1, max_sum = 1)
  p <- add.constraint(p, type = "box",
                      min = rep(0, 4), max = rep(0.1, 4))
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_error(validate_portfolio(p), "E13")
})

test_that("E13: sum(box_min) > max_sum detected", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 1, max_sum = 1)
  p <- add.constraint(p, type = "box",
                      min = rep(0.5, 4), max = rep(1, 4))
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_error(validate_portfolio(p), "E13")
})


# ---- E14: Group references out-of-range indices ----

test_that("E14: group index out of range detected", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "group",
                      groups = list(first = c(1, 2), second = c(3, 7)),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.9, 0.9))
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_error(validate_portfolio(p), "E14")
})


# ---- W1: No constraints ----

test_that("W1: no constraints emits warning", {
  p <- portfolio.spec(assets = funds)
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_warning(validate_portfolio(p), "W1")
})


# ---- W2: No objectives ----

test_that("W2: no objectives emits warning", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  expect_warning(validate_portfolio(p), "W2")
})


# ---- Multiple errors collected ----

test_that("Multiple errors are collected and reported together", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 1.5, max_sum = 0.5)
  # Box min > max for first asset
  p <- add.constraint(p, type = "box", min = c(0.8, 0, 0, 0),
                      max = c(0.2, 1, 1, 1))
  p <- add.objective(p, type = "risk", name = "StdDev")
  # Should collect E9, E10, and E13 (sum(box_min) = 0.8 > max_sum = 0.5)
  err <- tryCatch(validate_portfolio(p), error = function(e) e)
  expect_match(err$message, "E9")
  expect_match(err$message, "E10")
  expect_match(err$message, "E13")
  expect_match(err$message, "3 error")
})


# ---- Valid portfolio passes cleanly ----

test_that("Well-formed portfolio with R passes without error or warning", {
  p <- make_good_portf()
  expect_true(validate_portfolio(p))
  expect_true(validate_portfolio(p, R = R4))
})


# ---- Valid portfolio spec alone (no R) ----

test_that("validate_portfolio works without R argument", {
  p <- make_good_portf()
  expect_true(validate_portfolio(p))
})


# ---- Floating-point tolerance ----

test_that("Floating-point near-equal bounds do not trigger false positives", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 1, max_sum = 1)
  p <- add.constraint(p, type = "box",
                      min = rep(0, 4),
                      max = c(1 + .Machine$double.eps, 1, 1, 1))
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_true(validate_portfolio(p))
})
