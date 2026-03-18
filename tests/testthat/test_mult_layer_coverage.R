##### test_mult_layer_coverage.R #####
# Coverage: mult.layer.portfolio.R — proxy execution + negative weights

data(edhec, package = "PerformanceAnalytics")
R6 <- edhec[1:60, 1:6]
colnames(R6) <- paste0("A", 1:6)

# ===========================================================================
# 1. proxy.mult.portfolio executes with long-only portfolios (lines 149-161)
# ===========================================================================

test_that("proxy.mult.portfolio produces proxy returns", {
  top <- portfolio.spec(assets = c("proxy.1", "proxy.2"))
  top <- add.constraint(top, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
  top <- add.objective(top, type = "risk", name = "StdDev")

  sub1 <- portfolio.spec(assets = colnames(R6)[1:3])
  sub1 <- add.constraint(sub1, type = "weight_sum",
                         min_sum = 0.99, max_sum = 1.01)
  sub1 <- add.constraint(sub1, type = "box", min = 0.1, max = 0.8)
  sub1 <- add.objective(sub1, type = "risk", name = "StdDev")

  sub2 <- portfolio.spec(assets = colnames(R6)[4:6])
  sub2 <- add.constraint(sub2, type = "weight_sum",
                         min_sum = 0.99, max_sum = 1.01)
  sub2 <- add.constraint(sub2, type = "box", min = 0.1, max = 0.8)
  sub2 <- add.objective(sub2, type = "risk", name = "StdDev")

  mp <- mult.portfolio.spec(top)
  mp <- add.sub.portfolio(mp, sub1, optimize_method = "random",
                          search_size = 100, rebalance_on = "quarters",
                          training_period = 36)
  mp <- add.sub.portfolio(mp, sub2, optimize_method = "random",
                          search_size = 100, rebalance_on = "quarters",
                          training_period = 36)

  proxy_ret <- proxy.mult.portfolio(R6, mp)
  expect_true(inherits(proxy_ret, "xts"))
  expect_equal(ncol(proxy_ret), 2)
  expect_true(all(c("proxy.1", "proxy.2") %in% colnames(proxy_ret)))
})

# ===========================================================================
# 2. proxy.mult.portfolio with short-selling (geometric=FALSE, line 158-159)
# ===========================================================================

test_that("proxy.mult.portfolio handles negative weights with geometric=FALSE", {
  top <- portfolio.spec(assets = c("proxy.1", "proxy.2"))
  top <- add.constraint(top, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
  top <- add.objective(top, type = "risk", name = "StdDev")

  # Allow short positions
  sub1 <- portfolio.spec(assets = colnames(R6)[1:3])
  sub1 <- add.constraint(sub1, type = "weight_sum",
                         min_sum = 0.99, max_sum = 1.01)
  sub1 <- add.constraint(sub1, type = "box", min = -0.5, max = 1.5)
  sub1 <- add.objective(sub1, type = "risk", name = "StdDev")

  sub2 <- portfolio.spec(assets = colnames(R6)[4:6])
  sub2 <- add.constraint(sub2, type = "weight_sum",
                         min_sum = 0.99, max_sum = 1.01)
  sub2 <- add.constraint(sub2, type = "box", min = -0.5, max = 1.5)
  sub2 <- add.objective(sub2, type = "risk", name = "StdDev")

  mp <- mult.portfolio.spec(top)
  mp <- add.sub.portfolio(mp, sub1, optimize_method = "random",
                          search_size = 100, rebalance_on = "quarters",
                          training_period = 36)
  mp <- add.sub.portfolio(mp, sub2, optimize_method = "random",
                          search_size = 100, rebalance_on = "quarters",
                          training_period = 36)

  proxy_ret <- proxy.mult.portfolio(R6, mp)
  expect_true(inherits(proxy_ret, "xts"))
  expect_equal(ncol(proxy_ret), 2)
})
