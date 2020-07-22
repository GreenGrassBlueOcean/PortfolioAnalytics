
require(testthat)
require(PortfolioAnalytics)

context("constraints")

N <- 4
init.portf <- portfolio.spec(assets=N)
# Weight_sum constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="weight_sum", 
                             min_sum=0.99, 
                             max_sum=1.01)
# Box constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="box", 
                             min=0, 
                             max=1)
# Group constraint
init.portf <- add.constraint(portfolio=init.portf,
                             type="group",
                             groups=list(c(1, 3), c(2, 4)),
                             group_min=c(0.15, 0.25),
                             group_max=c(0.65, 0.55))
# Turnover constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="turnover", 
                             turnover_target=0.6)
# Diversification constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="diversification", 
                             div_target=0.55)
# Position limit constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="position_limit", 
                             max_pos=3, 
                             max_pos_long=2,
                             max_pos_short=1)
# Return constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="return", 
                             return_target=0.007)
# Factor exposure constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="factor_exposure",
                             B=rep(1, N),
                             lower=0.9, 
                             upper=1.1)

tmp_constraints <- PortfolioAnalytics:::get_constraints(init.portf)

test_that("weight_sum constraint is consistent", {
  expect_equal(tmp_constraints$min_sum, 0.99)
  expect_equal(tmp_constraints$max_sum, 1.01)
})

test_that("box constraint is consistent", {
  expect_equal(as.numeric(tmp_constraints$min), rep(0, N))
  expect_equal(as.numeric(tmp_constraints$max), rep(1, N))
})

test_that("group constraint is consistent", {
  expect_true(is.list(tmp_constraints$groups))
  expect_equal(tmp_constraints$groups[[1]], c(1, 3))
  expect_equal(tmp_constraints$groups[[2]], c(2, 4))
  expect_equal(tmp_constraints$group_labels, c("group1", "group2"))
  expect_equal(tmp_constraints$cLO, c(0.15, 0.25))
  expect_equal(tmp_constraints$cUP, c(0.65, 0.55))
})

test_that("turnover constraint is consistent", {
  expect_equal(tmp_constraints$turnover_target, 0.6)
})

test_that("diversification constraint is consistent", {
  expect_equal(tmp_constraints$div_target, 0.55)
})

test_that("position limit constraint is consistent", {
  expect_equal(tmp_constraints$max_pos, 3)
  expect_equal(tmp_constraints$max_pos_long, 2)
  expect_equal(tmp_constraints$max_pos_short, 1)
})

test_that("return constraint is consistent", {
  expect_equal(tmp_constraints$return_target, 0.007)
})


test_that("factor exposure constraint is consistent", {
  B <- matrix(1, ncol=1, nrow=N)
  rownames(B) <- paste("Asset", 1:N, sep=".")
  colnames(B) <- "factor1"
  
  expect_equal(tmp_constraints$B, B)
  expect_equal(tmp_constraints$lower, 0.9)
  expect_equal(tmp_constraints$upper, 1.1)
})
