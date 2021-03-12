test_that("te.target does nothing when everything is ok", {

  data("edhec")
  Equalweights <- rep(1/(ncol(edhec)-1), ncol(edhec)-1)
  test <- te.target( R = edhec[,1:12], Rb = edhec[,13],  weights = Equalweights
                  , min.te = 0.02, max.te = 0.05, scale = 12) 
  
  expect_equal(test, 0L)
})


test_that("te.target returns postive numeric when te > max.te", {
  
  data("edhec")
  Equalweights <- rep(1/(ncol(edhec)-1), ncol(edhec)-1)
  test <- te.target( R = edhec[,1:12], Rb = edhec[,13]*10,  weights = Equalweights
                     , min.te = 0.02, max.te = 0.05, scale = 12) 
  
  expect_equal(test, 4496.78366909737 )
})

test_that("te.target returns postive numeric when te < min.te", {
  
  data("edhec")
  Equalweights <- rep(1/(ncol(edhec)-1), ncol(edhec)-1)
  test <- te.target( R = edhec[,1:12], Rb = edhec[,13]*-0.0025,  weights = Equalweights
                     , min.te = 0.04, max.te = 0.05, scale = 12) 
  
  expect_equal(test, 23.3461183880688 )
})
