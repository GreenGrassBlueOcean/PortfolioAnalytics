##### test_stat_factor_model_coverage4.R #####
# Coverage: stat.factor.model.R — input validation stop() paths for SF/MF
# functions and "invalid k" branches in extract* methods.

# ===========================================================================
# 1. SF function dimension-mismatch errors
# ===========================================================================

test_that("covarianceSF rejects mismatched beta/stockM2 lengths", {
  expect_error(covarianceSF(beta = 1:4, stockM2 = 1:3, factorM2 = 1),
               "dimensions do not match")
})

test_that("coskewnessSF rejects mismatched beta/stockM3 lengths", {
  expect_error(coskewnessSF(beta = 1:4, stockM3 = 1:3, factorM3 = 1),
               "dimensions do not match")
})

test_that("cokurtosisSF rejects mismatched beta/stockM2 lengths", {
  expect_error(cokurtosisSF(beta = 1:4, stockM2 = 1:3, stockM4 = 1:4,
                             factorM2 = 1, factorM4 = 1),
               "dimensions do not match for beta and stockM2")
})

test_that("cokurtosisSF rejects mismatched beta/stockM4 lengths", {
  expect_error(cokurtosisSF(beta = 1:4, stockM2 = 1:4, stockM4 = 1:3,
                             factorM2 = 1, factorM4 = 1),
               "dimensions do not match for beta and stockM4")
})

# ===========================================================================
# 2. MF function validation errors
# ===========================================================================

test_that("covarianceMF rejects non-matrix beta", {
  expect_error(covarianceMF(beta = 1:4, stockM2 = 1:4, factorM2 = matrix(1)),
               "beta must be a matrix")
})

test_that("covarianceMF rejects mismatched stockM2 length", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(covarianceMF(beta, stockM2 = 1:3, factorM2 = diag(2)),
               "dimensions do not match for beta and stockM2")
})

test_that("covarianceMF rejects non-matrix factorM2", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(covarianceMF(beta, stockM2 = 1:4, factorM2 = 1:4),
               "factorM2 must be a matrix")
})

test_that("covarianceMF rejects wrong-dimension factorM2", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(covarianceMF(beta, stockM2 = 1:4, factorM2 = diag(3)),
               "dimensions do not match for beta and factorM2")
})

test_that("coskewnessMF rejects non-matrix beta", {
  expect_error(coskewnessMF(beta = 1:4, stockM3 = 1:4, factorM3 = matrix(1)),
               "beta must be a matrix")
})

test_that("coskewnessMF rejects mismatched stockM3", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(coskewnessMF(beta, stockM3 = 1:3, factorM3 = matrix(1:4, 2, 4)),
               "dimensions do not match for beta and stockM3")
})

test_that("coskewnessMF rejects non-matrix factorM3", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(coskewnessMF(beta, stockM3 = 1:4, factorM3 = 1:4),
               "factorM3 must be a matrix")
})

test_that("coskewnessMF rejects wrong-dimension factorM3", {
  beta <- matrix(1:8, ncol = 2)
  # k=2 so factorM3 should be (2 x 4), pass (2 x 3)
  expect_error(coskewnessMF(beta, stockM3 = 1:4, factorM3 = matrix(1:6, 2, 3)),
               "dimensions do not match for beta and factorM3")
})

test_that("cokurtosisMF rejects non-matrix beta", {
  expect_error(cokurtosisMF(beta = 1:4, stockM2 = 1:4, stockM4 = 1:4,
                             factorM2 = diag(1), factorM4 = matrix(1)),
               "beta must be a matrix")
})

test_that("cokurtosisMF rejects mismatched stockM2", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(cokurtosisMF(beta, stockM2 = 1:3, stockM4 = 1:4,
                             factorM2 = diag(2), factorM4 = matrix(1:16, 2, 8)),
               "dimensions do not match for beta and stockM2")
})

test_that("cokurtosisMF rejects mismatched stockM4", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(cokurtosisMF(beta, stockM2 = 1:4, stockM4 = 1:3,
                             factorM2 = diag(2), factorM4 = matrix(1:16, 2, 8)),
               "dimensions do not match for beta and stockM4")
})

test_that("cokurtosisMF rejects non-matrix factorM2", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(cokurtosisMF(beta, stockM2 = 1:4, stockM4 = 1:4,
                             factorM2 = 1:4, factorM4 = matrix(1:16, 2, 8)),
               "factorM2 must be a matrix")
})

test_that("cokurtosisMF rejects wrong-dimension factorM2", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(cokurtosisMF(beta, stockM2 = 1:4, stockM4 = 1:4,
                             factorM2 = diag(3), factorM4 = matrix(1:16, 2, 8)),
               "dimensions do not match for beta and factorM2")
})

test_that("cokurtosisMF rejects non-matrix factorM4", {
  beta <- matrix(1:8, ncol = 2)
  expect_error(cokurtosisMF(beta, stockM2 = 1:4, stockM4 = 1:4,
                             factorM2 = diag(2), factorM4 = 1:8),
               "factorM4 must be a matrix")
})

test_that("cokurtosisMF rejects wrong-dimension factorM4", {
  beta <- matrix(1:8, ncol = 2)
  # k=2 so factorM4 should be (2 x 8), pass (2 x 4)
  expect_error(cokurtosisMF(beta, stockM2 = 1:4, stockM4 = 1:4,
                             factorM2 = diag(2), factorM4 = matrix(1:8, 2, 4)),
               "dimensions do not match for beta and factorM4")
})

# ===========================================================================
# 3. .residualcokurtosisSF / .residualcokurtosisMF wrapper validation
# ===========================================================================

test_that(".residualcokurtosisSF rejects wrong-length inputs", {
  expect_error(.residualcokurtosisSF(NN = 3L, sstockM2 = 1:2,
                                      sstockM4 = 1:3, mfactorM2 = 1.0,
                                      bbeta = 1:3),
               "sstockM2 must be a vector of length NN")
  expect_error(.residualcokurtosisSF(NN = 3L, sstockM2 = 1:3,
                                      sstockM4 = 1:2, mfactorM2 = 1.0,
                                      bbeta = 1:3),
               "sstockM4 must be a vector of length NN")
  expect_error(.residualcokurtosisSF(NN = 3L, sstockM2 = 1:3,
                                      sstockM4 = 1:3, mfactorM2 = 1.0,
                                      bbeta = 1:2),
               "bbeta must be a vector of length NN")
})

test_that(".residualcokurtosisMF rejects wrong-length inputs", {
  expect_error(.residualcokurtosisMF(NN = 3L, sstockM2 = 1:2,
                                      sstockM4 = 1:3, bbetacov = rep(1, 9)),
               "sstockM2 must be a vector of length NN")
  expect_error(.residualcokurtosisMF(NN = 3L, sstockM2 = 1:3,
                                      sstockM4 = 1:2, bbetacov = rep(1, 9)),
               "sstockM4 must be a vector of length NN")
  expect_error(.residualcokurtosisMF(NN = 3L, sstockM2 = 1:3,
                                      sstockM4 = 1:3, bbetacov = rep(1, 4)),
               "bbetacov must be a vector of length NN\\*NN")
})

# ===========================================================================
# 4. .residualcokurtosisSF coercion paths (non-integer NN, non-double factorM2)
# ===========================================================================

test_that(".residualcokurtosisSF coerces non-integer NN and non-double factorM2", {
  # Pass NN as double and factorM2 as integer — should coerce without error
  result <- .residualcokurtosisSF(NN = 2, sstockM2 = c(0.1, 0.2),
                                   sstockM4 = c(0.01, 0.02),
                                   mfactorM2 = 1L, bbeta = c(0.5, 0.3))
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2, 8))
})

test_that(".residualcokurtosisMF coerces non-integer NN", {
  result <- .residualcokurtosisMF(NN = 2, sstockM2 = c(0.1, 0.2),
                                   sstockM4 = c(0.01, 0.02),
                                   bbetacov = rep(0.1, 4))
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2, 8))
})

# ===========================================================================
# 5. extract* "invalid k" branches
# ===========================================================================

test_that("extractCovariance returns NULL with message for invalid k", {
  sfm <- statistical.factor.model(edhec[1:60, 1:4], k = 1)
  # Manually set k to 0 to trigger the else branch
  sfm$k <- 0
  expect_message(result <- extractCovariance(sfm), "invalid k")
  expect_null(result)
})

test_that("extractCoskewness returns NULL with message for invalid k", {
  sfm <- statistical.factor.model(edhec[1:60, 1:4], k = 1)
  sfm$k <- 0
  expect_message(result <- extractCoskewness(sfm), "invalid k")
  expect_null(result)
})

test_that("extractCokurtosis returns NULL with message for invalid k", {
  sfm <- statistical.factor.model(edhec[1:60, 1:4], k = 1)
  sfm$k <- 0
  expect_message(result <- extractCokurtosis(sfm), "invalid k")
  expect_null(result)
})
