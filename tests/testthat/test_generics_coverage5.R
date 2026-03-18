##### test_generics_coverage5.R #####
# Coverage: generics.R — summary.optimize.portfolio constraint reporting paths

skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

data(edhec, package = "PerformanceAnalytics")
R4 <- edhec[1:48, 1:4]
colnames(R4) <- c("A", "B", "C", "D")

# ===========================================================================
# Shared base portfolio
# ===========================================================================

base_portf <- portfolio.spec(assets = colnames(R4))
base_portf <- add.constraint(base_portf, type = "weight_sum",
                             min_sum = 0.99, max_sum = 1.01)
base_portf <- add.constraint(base_portf, type = "box", min = 0.05, max = 0.65)
base_portf <- add.objective(base_portf, type = "risk", name = "StdDev")
base_portf <- add.objective(base_portf, type = "return", name = "mean")

# ===========================================================================
# 1. summary with group constraints (lines 706-726)
# ===========================================================================

test_that("summary prints group constraint section", {
  portf_g <- add.constraint(base_portf, type = "group",
                            groups = list(g1 = 1:2, g2 = 3:4),
                            group_min = c(0.2, 0.2),
                            group_max = c(0.8, 0.8))
  opt <- suppressWarnings(optimize.portfolio(R4, portf_g,
                                             optimize_method = "ROI"))
  out <- capture.output(print(summary(opt)))
  expect_true(any(grepl("Group Constraints", out)))
  expect_true(any(grepl("Group Weights", out)))
})

# ===========================================================================
# 2. summary with position_limit all NULL → "Unconstrained" (lines 732-759)
# ===========================================================================

test_that("summary prints Unconstrained for missing position limits", {
  opt <- suppressWarnings(optimize.portfolio(R4, base_portf,
                                             optimize_method = "ROI"))
  out <- capture.output(print(summary(opt)))
  unconstrained_lines <- grep("Unconstrained", out)
  # max_pos, max_pos_long, max_pos_short all NULL → 3 "Unconstrained" lines

  expect_gte(length(unconstrained_lines), 3)
})

# ===========================================================================
# 3. summary with diversification_target (lines 762-771)
# ===========================================================================

test_that("summary prints diversification target when set", {
  portf_div <- add.constraint(base_portf, type = "diversification",
                              div_target = 0.7)
  opt <- suppressWarnings(optimize.portfolio(R4, portf_div,
                                             optimize_method = "ROI"))
  out <- capture.output(print(summary(opt)))
  expect_true(any(grepl("Diversification Target", out)))
  expect_true(any(grepl("0\\.7", out)))
})

# ===========================================================================
# 4. summary with turnover_target (lines 774-783)
# ===========================================================================

test_that("summary prints turnover target when set", {
  portf_to <- add.constraint(base_portf, type = "turnover",
                             turnover_target = 0.3)
  opt <- suppressWarnings(optimize.portfolio(R4, portf_to,
                                             optimize_method = "ROI"))
  out <- capture.output(print(summary(opt)))
  expect_true(any(grepl("Turnover Target", out)))
  expect_true(any(grepl("0\\.3", out)))
})

# ===========================================================================
# 5. summary with factor exposure constraint (lines 786-800)
# ===========================================================================

test_that("summary prints factor exposure section", {
  B <- matrix(runif(4), nrow = 4, ncol = 1,
              dimnames = list(colnames(R4), "Factor1"))
  portf_fe <- add.constraint(base_portf, type = "factor_exposure",
                             B = B, lower = 0.1, upper = 0.9)
  opt <- suppressWarnings(optimize.portfolio(R4, portf_fe,
                                             optimize_method = "ROI"))
  out <- capture.output(print(summary(opt)))
  expect_true(any(grepl("Factor Exposure", out)))
  expect_true(any(grepl("Factor1", out)))
})

# ===========================================================================
# 6. summary with category_labels only, no groups (lines 888-894)
# ===========================================================================

test_that("summary includes category weights when category_labels set", {
  portf_cat <- base_portf
  portf_cat$category_labels <- list(Equity = c("A", "B"), Fixed = c("C", "D"))
  opt <- suppressWarnings(optimize.portfolio(R4, portf_cat,
                                             optimize_method = "ROI"))
  s <- summary(opt)
  # category_weights should be populated in the summary object
  expect_false(is.null(s$category_weights))
})
