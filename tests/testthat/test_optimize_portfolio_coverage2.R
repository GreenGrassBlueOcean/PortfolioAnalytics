context("optimize.portfolio.R coverage: additional paths")

# Shared setup: lightweight ROI optimization
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:36, 1:5]
R3 <- R5[, 1:3]

make_portf <- function(assets) {
  p <- portfolio.spec(assets = assets)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.8)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
}

# --- R has more columns than portfolio assets (L877-879) ----------------------

test_that("optimize.portfolio subsets R when ncol(R) > N", {
  portf <- make_portf(colnames(R3))
  opt <- optimize.portfolio(R5, portf, optimize_method = "ROI")
  expect_equal(length(opt$weights), 3)
  expect_equal(names(opt$weights), colnames(R3))
})

# --- momentFUN try-error path (L924-925) --------------------------------------
# Note: when momentFUN fails, the code messages the failure but dotargs is
# never assigned, causing a downstream error. This test covers L924-925
# (the message path) while accepting the subsequent error.

test_that("optimize.portfolio messages when momentFUN fails", {
  portf <- make_portf(colnames(R3))
  bad_moment <- function(R, portfolio, ...) stop("moment failure")
  msgs <- character()
  tryCatch(
    withCallingHandlers(
      optimize.portfolio(R3, portf, optimize_method = "ROI",
                         momentFUN = bad_moment),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) NULL
  )
  expect_true(any(grepl("portfolio moment function failed", msgs)))
})

# --- portfolio.list path in optimize.portfolio (L781-802) ---------------------

test_that("optimize.portfolio handles portfolio.list input", {
  portf1 <- make_portf(colnames(R3))
  portf2 <- portfolio.spec(assets = colnames(R3))
  portf2 <- add.constraint(portf2, type = "full_investment")
  portf2 <- add.constraint(portf2, type = "box", min = 0.1, max = 0.7)
  portf2 <- add.objective(portf2, type = "return", name = "mean")

  portf_list <- combine.portfolios(list(portf1, portf2))
  opt <- optimize.portfolio(R3, portf_list, optimize_method = "ROI")
  expect_s3_class(opt, "opt.list")
  expect_equal(length(opt), 2)
})

# --- message=TRUE branches ---------------------------------------------------

test_that("optimize.portfolio with message=TRUE produces output", {
  portf <- make_portf(colnames(R3))
  expect_message(
    opt <- optimize.portfolio(R3, portf, optimize_method = "ROI",
                              message = TRUE),
    "elapsed time"
  )
  expect_s3_class(opt, "optimize.portfolio")
})

# --- trailing_periods backward compat in rebalancing --------------------------

test_that("optimize.portfolio.rebalancing supports trailing_periods argument", {
  portf <- make_portf(colnames(R3))
  opt_rebal <- optimize.portfolio.rebalancing(
    R3, portf,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24,
    trailing_periods = 30
  )
  expect_s3_class(opt_rebal, "optimize.portfolio.rebalancing")
})

# --- portfolio.list path in optimize.portfolio.rebalancing --------------------

test_that("optimize.portfolio.rebalancing handles portfolio.list input", {
  portf1 <- make_portf(colnames(R3))
  portf2 <- portfolio.spec(assets = colnames(R3))
  portf2 <- add.constraint(portf2, type = "full_investment")
  portf2 <- add.constraint(portf2, type = "box", min = 0.1, max = 0.7)
  portf2 <- add.objective(portf2, type = "return", name = "mean")

  portf_list <- combine.portfolios(list(portf1, portf2))
  opt_rebal <- optimize.portfolio.rebalancing(
    R3, portf_list,
    optimize_method = "ROI",
    rebalance_on = "quarters",
    training_period = 24
  )
  expect_s3_class(opt_rebal, "opt.rebal.list")
})
