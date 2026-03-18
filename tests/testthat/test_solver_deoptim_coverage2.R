##### test_solver_deoptim_coverage2.R #####
# Coverage: solver_deoptim.R — NP clamping, rp initial population, warm_start

skip_if_not_installed("DEoptim")

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]
colnames(R3) <- c("A", "B", "C")

portf <- portfolio.spec(assets = colnames(R3))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
portf <- add.objective(portf, type = "risk", name = "StdDev")

# ===========================================================================
# 1. NP >= 2000 cap (line 25): large search_size forces NP clamping
# ===========================================================================

test_that("DEoptim clamps NP to 2000 for large search_size", {
  # With itermax=5 and search_size=50000, NP = 50000/5 = 10000, capped to 2000
  opt <- optimize.portfolio(R3, portf, optimize_method = "DEoptim",
                             trace = TRUE, traceDE = 0,
                             search_size = 50000, itermax = 5)
  expect_true(!is.null(opt$weights))
  expect_equal(sum(opt$weights), 1, tolerance = 0.02)
})

# ===========================================================================
# 2. Pre-computed rp matrix as initial population (lines 93-95)
# ===========================================================================

test_that("DEoptim uses provided rp matrix as initial population", {
  rp_matrix <- random_portfolios(portf, permutations = 50, rp_method = "sample")
  opt <- optimize.portfolio(R3, portf, optimize_method = "DEoptim",
                             trace = TRUE, traceDE = 0, rp = rp_matrix,
                             DEoptim.control = list(itermax = 5))
  expect_true(!is.null(opt$weights))
  expect_equal(sum(opt$weights), 1, tolerance = 0.02)
})

# ===========================================================================
# 3. Warm-start injection into initial population (line 106)
# ===========================================================================

test_that("DEoptim warm_start injects weights into initial population", {
  ws <- c(0.4, 0.35, 0.25)
  names(ws) <- colnames(R3)
  opt <- optimize.portfolio(R3, portf, optimize_method = "DEoptim",
                             trace = TRUE, traceDE = 0,
                             warm_start = ws,
                             DEoptim.control = list(itermax = 10))
  expect_true(!is.null(opt$weights))
  expect_equal(sum(opt$weights), 1, tolerance = 0.02)
})
