
require(testthat)
require(PortfolioAnalytics)

context("print and summary generics")

# ---- Shared test data ----
data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

portf <- portfolio.spec(assets = colnames(R5))
portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.55)
portf <- add.constraint(portf, type = "group",
                        groups = list(c(1, 2), c(3, 4, 5)),
                        group_min = c(0.1, 0.15),
                        group_max = c(0.6, 0.85))
portf <- add.objective(portf, type = "return", name = "mean")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# ---- print.portfolio ----

test_that("print.portfolio runs without error", {
  expect_output(print(portf), "Portfolio")
})

# ---- summary.portfolio ----

test_that("summary.portfolio runs without error", {
  s <- summary(portf)
  expect_output(print(s), "")
})

# ---- print.constraint ----

test_that("print.constraint runs without error", {
  constr <- portf$constraints[[1]]
  expect_output(print(constr))
})

# ---- print/summary for optimize.portfolio.random ----

test_that("print and summary work for random optimization", {
  set.seed(8127)
  opt <- optimize.portfolio(R5, portf, optimize_method = "random",
                            search_size = 300, trace = TRUE)

  expect_output(print(opt))
  s <- summary(opt)
  expect_output(print(s), "")

  # summary fields
  expect_true(!is.null(s$weights))
  expect_true(!is.null(s$objective_values))
  expect_true(!is.null(s$leverage_constraint))
  expect_true(!is.null(s$box_constraint))
  expect_true(!is.null(s$group_constraint))
})

# ---- print/summary for optimize.portfolio.ROI ----

test_that("print and summary work for ROI optimization", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_roi <- portfolio.spec(assets = colnames(R5))
  portf_roi <- add.constraint(portf_roi, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf_roi <- add.constraint(portf_roi, type = "box", min = 0.05, max = 0.55)
  portf_roi <- add.objective(portf_roi, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R5, portf_roi, optimize_method = "ROI", trace = TRUE)

  expect_output(print(opt))
  s <- summary(opt)
  expect_output(print(s), "")
})

# ---- print/summary for optimize.portfolio.pso ----

test_that("print and summary work for PSO optimization", {
  skip_if_not_installed("pso")

  set.seed(3341)
  opt <- optimize.portfolio(R5, portf, optimize_method = "pso", trace = TRUE)

  expect_output(print(opt))
  s <- summary(opt)
  expect_output(print(s), "")
})

# ---- print/summary for optimize.portfolio.GenSA ----

test_that("print and summary work for GenSA optimization", {
  skip_if_not_installed("GenSA")

  set.seed(5529)
  opt <- optimize.portfolio(R5, portf, optimize_method = "GenSA", trace = TRUE)

  expect_output(print(opt))
  s <- summary(opt)
  expect_output(print(s), "")
})

# ---- print/summary for optimize.portfolio.DEoptim ----

test_that("print and summary work for DEoptim optimization", {
  skip_if_not_installed("DEoptim")

  set.seed(1482)
  opt <- optimize.portfolio(R5, portf, optimize_method = "DEoptim", trace = TRUE,
                            search_size = 500)

  expect_output(print(opt))
  s <- summary(opt)
  expect_output(print(s), "")
})

# ---- print/summary for optimize.portfolio.rebalancing ----

test_that("print and summary work for rebalancing object", {
  set.seed(2904)
  opt_rebal <- optimize.portfolio.rebalancing(R5, portf,
                                              optimize_method = "random",
                                              search_size = 200,
                                              rebalance_on = "quarters",
                                              training_period = 24,
                                              trace = TRUE)

  expect_output(print(opt_rebal))
  s <- summary(opt_rebal)
  expect_output(print(s), "")
})

# ---- print/summary for efficient.frontier ----

test_that("print and summary work for efficient frontier", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  portf_ef <- portfolio.spec(assets = colnames(R5))
  portf_ef <- add.constraint(portf_ef, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf_ef <- add.constraint(portf_ef, type = "box", min = 0.05, max = 0.55)
  portf_ef <- add.objective(portf_ef, type = "return", name = "mean")
  portf_ef <- add.objective(portf_ef, type = "risk", name = "StdDev")

  ef <- create.EfficientFrontier(R5, portf_ef, type = "mean-StdDev", n.portfolios = 10)

  expect_output(print(ef))
  s <- summary(ef)
  expect_true(!is.null(s))
})
