##### test_solver_deoptim_coverage.R #####
# Coverage: solver_deoptim.R — warm start, optional control params

skip_if_not_installed("DEoptim")

data(edhec, package = "PerformanceAnalytics")
R3 <- edhec[1:36, 1:3]

portf <- portfolio.spec(assets = colnames(R3))
portf <- add.constraint(portf, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
portf <- add.objective(portf, type = "risk", name = "ES")

# ===========================================================================
# 1. DEoptim with warm_start (population injection)
# ===========================================================================

test_that("DEoptim optimization with warm_start", {
  ws <- c(0.4, 0.3, 0.3)
  names(ws) <- colnames(R3)
  set.seed(5617)
  opt <- suppressWarnings(suppressMessages(
    optimize.portfolio(R3, portf, optimize_method = "DEoptim",
                       trace = TRUE, warm_start = ws,
                       DEoptim.control = list(itermax = 10, trace = FALSE))
  ))
  expect_true(!is.null(opt$weights))
  expect_equal(length(opt$weights), 3)
})

# ===========================================================================
# 2. DEoptim with custom control parameters
# ===========================================================================

test_that("DEoptim with custom control parameters", {
  set.seed(5618)
  opt <- suppressWarnings(suppressMessages(
    optimize.portfolio(R3, portf, optimize_method = "DEoptim",
                       trace = TRUE,
                       DEoptim.control = list(
                         itermax = 10, trace = FALSE,
                         strategy = 3, reltol = 1e-5, steptol = 5
                       ))
  ))
  expect_true(!is.null(opt$weights))
})
