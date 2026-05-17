##### test_objective_coverage.R #####
# Coverage tests for objective.R: error paths, edge cases, insert_objectives

data(edhec)
R4 <- edhec[1:60, 1:4]
colnames(R4) <- c("CA", "CTAG", "DS", "EM")
funds <- colnames(R4)

base_portf <- function() {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p
}

# ===========================================================================
# 1. objective() constructor error paths
# ===========================================================================
pa_objective <- getExportedValue("PortfolioAnalytics", "objective")

test_that("objective() errors when name is missing", {
  expect_error(pa_objective(), "you must specify an objective name")
})

test_that("objective() errors when name is NULL", {
  expect_error(pa_objective(name = NULL), "you must specify an objective name")
})

test_that("objective() errors when arguments is not a list", {
  expect_error(
    pa_objective(name = "mean", arguments = "bad"),
    "arguments must be passed as a named list"
  )
})

# ===========================================================================
# 2. add.objective error / edge paths
# ===========================================================================

test_that("add.objective errors when portfolio is not a portfolio object", {
  expect_error(
    add.objective(portfolio = list(), type = "return", name = "mean"),
    "not of class portfolio"
  )
})

test_that("add.objective errors when name is missing for non-QU type", {
  p <- base_portf()
  expect_error(
    add.objective(portfolio = p, type = "return"),
    "you must supply a name"
  )
})

test_that("add.objective errors when arguments is not a list", {
  p <- base_portf()
  expect_error(
    add.objective(portfolio = p, type = "return", name = "mean",
                  arguments = "bad"),
    "arguments must be passed as a named list"
  )
})

test_that("add.objective with type='null' returns portfolio unchanged", {
  p <- base_portf()
  p2 <- add.objective(p, type = "null", name = "whatever")
  expect_equal(length(p2$objectives), 0)
})

test_that("add.objective with indexnum updates specific slot", {
  p <- base_portf()
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk", name = "StdDev")
  # Overwrite the first objective
  p <- add.objective(p, type = "risk", name = "ES", indexnum = 1)
  expect_equal(p$objectives[[1]]$name, "ES")
  expect_equal(length(p$objectives), 2)
})

# ===========================================================================
# 3. risk_budget_objective validation
# ===========================================================================

test_that("risk_budget_objective errors on mismatched min/max_prisk lengths", {
  p <- base_portf()
  expect_error(
    add.objective(p, type = "risk_budget", name = "StdDev",
                  min_prisk = c(0.1, 0.2), max_prisk = c(0.3, 0.4, 0.5)),
    "length of min_prisk and max_prisk must be the same"
  )
})

test_that("risk_budget_objective errors when min_prisk has wrong length", {
  p <- base_portf()
  expect_error(
    add.objective(p, type = "risk_budget", name = "StdDev",
                  min_prisk = c(0.1, 0.2)),
    "length of min_prisk must be equal to 1 or the number of assets"
  )
})

test_that("risk_budget_objective errors when max_prisk has wrong length", {
  p <- base_portf()
  expect_error(
    add.objective(p, type = "risk_budget", name = "StdDev",
                  max_prisk = c(0.1, 0.2)),
    "length of max_prisk must be equal to 1 or the number of assets"
  )
})

test_that("risk_budget_objective replicates scalar min/max_prisk", {
  p <- base_portf()
  p <- add.objective(p, type = "risk_budget", name = "StdDev",
                     min_prisk = 0.1, max_prisk = 0.5)
  obj <- p$objectives[[1]]
  expect_equal(length(obj$min_prisk), 4)
  expect_equal(length(obj$max_prisk), 4)
  expect_true(all(obj$min_prisk == 0.1))
  expect_true(all(obj$max_prisk == 0.5))
})

# ===========================================================================
# 4. weight_concentration_objective validation
# ===========================================================================

test_that("weight_concentration_objective errors when conc_groups is not a list", {
  p <- base_portf()
  expect_error(
    add.objective(p, type = "weight_concentration", name = "HHI",
                  conc_aversion = 0.1, conc_groups = c(1, 2)),
    "conc_groups must be passed in as a list"
  )
})

test_that("weight_concentration_objective errors on length mismatch", {
  p <- base_portf()
  expect_error(
    add.objective(p, type = "weight_concentration", name = "HHI",
                  conc_aversion = c(0.1, 0.2, 0.3),
                  conc_groups = list(c(1, 2), c(3, 4))),
    "length of conc_aversion must be equal to length of groups"
  )
})

test_that("weight_concentration_objective errors when conc_aversion is vector without groups", {
  p <- base_portf()
  expect_error(
    add.objective(p, type = "weight_concentration", name = "HHI",
                  conc_aversion = c(0.1, 0.2)),
    "conc_aversion must be a scalar"
  )
})

test_that("weight_concentration_objective replicates scalar conc_aversion to groups", {
  p <- base_portf()
  p <- add.objective(p, type = "weight_concentration", name = "HHI",
                     conc_aversion = 0.1,
                     conc_groups = list(c(1, 2), c(3, 4)))
  obj <- p$objectives[[1]]
  expect_equal(length(obj$conc_aversion), 2)
  expect_true(all(obj$conc_aversion == 0.1))
})

# ===========================================================================
# 5. insert_objectives error paths
# ===========================================================================

test_that("insert_objectives errors on non-portfolio input", {
  expect_error(
    insert_objectives(portfolio = list(), objectives = list()),
    "you must pass in an object of class portfolio"
  )
})

test_that("insert_objectives errors when objectives is not a list", {
  p <- base_portf()
  expect_error(
    insert_objectives(portfolio = p, objectives = "bad"),
    "objectives must be passed in as a list"
  )
})

test_that("insert_objectives errors when list contains non-objective", {
  p <- base_portf()
  expect_error(
    insert_objectives(portfolio = p, objectives = list("not_an_objective")),
    "all objects in objectives must be of class"
  )
})

test_that("insert_objectives sets objectives on valid input", {
  p <- base_portf()
  obj <- return_objective(name = "mean")
  p2 <- insert_objectives(p, list(obj))
  expect_equal(length(p2$objectives), 1)
  expect_equal(p2$objectives[[1]]$name, "mean")
})
