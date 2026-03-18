##### test_portfolio_coverage.R #####
# Coverage tests for portfolio.R: error paths, message=TRUE, regime.portfolios validation

data(edhec)
R4 <- edhec[1:60, 1:4]
colnames(R4) <- c("CA", "CTAG", "DS", "EM")
funds <- colnames(R4)

# ===========================================================================
# 1. portfolio.spec error / edge cases
# ===========================================================================

test_that("portfolio.spec errors when assets is NULL", {
  expect_error(portfolio.spec(assets = NULL), "You must specify the assets")
})

test_that("portfolio.spec message=TRUE with numeric scalar", {
  expect_message(
    portfolio.spec(assets = 3, message = TRUE),
    "assuming equal weighted"
  )
})

test_that("portfolio.spec message=TRUE with character vector", {
  expect_message(
    portfolio.spec(assets = c("A", "B", "C"), message = TRUE),
    "assuming equal weighted"
  )
})

test_that("portfolio.spec with numeric vector uses values as weights", {
  w <- c(A = 0.5, B = 0.3, C = 0.2)
  p <- portfolio.spec(assets = w)
  expect_equal(p$assets, w)
  expect_equal(names(p$assets), c("A", "B", "C"))
})

test_that("portfolio.spec auto-names unnamed numeric vector", {
  p <- portfolio.spec(assets = c(0.5, 0.3, 0.2))
  expect_equal(names(p$assets), c("Asset.1", "Asset.2", "Asset.3"))
})

# ===========================================================================
# 2. category_labels validation
# ===========================================================================

test_that("portfolio.spec errors when category_labels is not character", {
  expect_error(
    portfolio.spec(assets = funds, category_labels = c(1, 2, 3, 4)),
    "category_labels must be a character"
  )
})

test_that("portfolio.spec errors when category_labels length mismatches assets", {
  expect_error(
    portfolio.spec(assets = funds, category_labels = c("EQ", "FI")),
    "length.*category_labels.*must be equal"
  )
})

# ===========================================================================
# 3. regime.portfolios validation
# ===========================================================================

test_that("regime.portfolios errors when regime is not xts/zoo", {
  p1 <- portfolio.spec(assets = funds)
  p2 <- portfolio.spec(assets = funds)
  portf_list <- combine.portfolios(list(p1, p2))
  
  expect_error(
    regime.portfolios(regime = c(1, 1, 2, 2), portfolios = portf_list),
    "regime object must be an xts or zoo"
  )
})

test_that("regime.portfolios errors when portfolios is not portfolio.list", {
  regime_ts <- xts::xts(c(1, 2), order.by = as.Date(c("2020-01-01", "2020-02-01")))
  
  expect_error(
    regime.portfolios(regime = regime_ts, portfolios = list()),
    "portfolios object must be a portfolio.list"
  )
})

test_that("regime.portfolios errors when regime/portfolio count mismatch", {
  p1 <- portfolio.spec(assets = funds)
  p2 <- portfolio.spec(assets = funds)
  portf_list <- combine.portfolios(list(p1, p2))
  regime_ts <- xts::xts(c(1, 2, 3), order.by = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")))
  
  expect_error(
    regime.portfolios(regime = regime_ts, portfolios = portf_list),
    "Number of portfolios must match"
  )
})

test_that("regime.portfolios errors when portfolio assets differ", {
  p1 <- portfolio.spec(assets = c("A", "B"))
  p2 <- portfolio.spec(assets = c("X", "Y"))
  portf_list <- combine.portfolios(list(p1, p2))
  regime_ts <- xts::xts(c(1, 2), order.by = as.Date(c("2020-01-01", "2020-02-01")))
  
  expect_error(
    regime.portfolios(regime = regime_ts, portfolios = portf_list),
    "assets in each portfolio must be identical"
  )
})
