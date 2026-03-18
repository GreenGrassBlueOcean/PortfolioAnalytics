##### test_optFUN_coverage6.R #####
# Regression: optFUN.R — NULL cLO/cUP group constraint fallback.
# Before fix, NULL cLO caused silent rhs.vec truncation and NULL cUP
# caused "invalid argument to unary operator" (-NULL error).

library(ROI)
library(ROI.plugin.quadprog)

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:60, 1:4]

# Shared setup: portfolio with group constraints
p <- portfolio.spec(assets = colnames(R4))
p <- add.constraint(p, type = "full_investment")
p <- add.constraint(p, type = "long_only")
p <- add.constraint(p, type = "group",
                    groups = list(1:2, 3:4),
                    group_min = c(0.2, 0.2),
                    group_max = c(0.8, 0.8))
p <- add.objective(p, type = "risk", name = "var")

cstr <- get_constraints(p)
moments <- list(mean = colMeans(R4), var = cov(as.matrix(R4)))

test_that("gmv_opt handles NULL cLO with -Inf fallback", {
  cstr_mod <- cstr
  cstr_mod$cLO <- NULL
  result <- gmv_opt(R = R4, constraints = cstr_mod, moments = moments,
                    lambda = 1, target = NA, lambda_hhi = 0, conc_groups = NULL)
  expect_true(all(!is.na(result$weights)))
  expect_equal(length(result$weights), 4)
})

test_that("gmv_opt handles NULL cUP with Inf fallback", {
  cstr_mod <- cstr
  cstr_mod$cUP <- NULL
  result <- gmv_opt(R = R4, constraints = cstr_mod, moments = moments,
                    lambda = 1, target = NA, lambda_hhi = 0, conc_groups = NULL)
  expect_true(all(!is.na(result$weights)))
  expect_equal(length(result$weights), 4)
})

test_that("gmv_opt handles both cLO and cUP NULL", {
  cstr_mod <- cstr
  cstr_mod$cLO <- NULL
  cstr_mod$cUP <- NULL
  result <- gmv_opt(R = R4, constraints = cstr_mod, moments = moments,
                    lambda = 1, target = NA, lambda_hhi = 0, conc_groups = NULL)
  expect_true(all(!is.na(result$weights)))
  expect_equal(length(result$weights), 4)
})
