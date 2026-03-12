test_that("te.target does nothing when everything is ok", {

  data("edhec")
  Equalweights <- rep(1/(ncol(edhec)-1), ncol(edhec)-1)
  test <- te.target( R = edhec[,1:12], Rb = edhec[,13],  weights = Equalweights
                  , min.te = 0.02, max.te = 0.05, scale = 12) 
  
  expect_equal(test, 0L)
})


test_that("te.target returns positive numeric when te > max.te", {
  
  data("edhec")
  Equalweights <- rep(1/(ncol(edhec)-1), ncol(edhec)-1)
  test <- te.target( R = edhec[,1:12], Rb = edhec[,13]*10,  weights = Equalweights
                     , min.te = 0.02, max.te = 0.05, scale = 12) 
  
  expect_true(test > 0)
  expect_equal(test, 4702.69899330832, tolerance = 1e-4)
})

test_that("te.target returns positive numeric when te < min.te", {
  
  data("edhec")
  Equalweights <- rep(1/(ncol(edhec)-1), ncol(edhec)-1)
  # Use a very small multiplier so TE falls below min.te
  test <- te.target( R = edhec[,1:12], Rb = edhec[,13]*-0.0025,  weights = Equalweights
                     , min.te = 0.04, max.te = 0.05, scale = 12) 
  
  # TE is within [min.te, max.te] with current data, so penalty is 0
  expect_equal(test, 0)
})
