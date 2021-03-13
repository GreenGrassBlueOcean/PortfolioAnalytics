test_that("ranking.moments works with AC moments", {
  
  
  data("edhec")
  AC_Moments <- ranking.moments(edhec[,1:4],n=3, method  = "ac")
  
  AC_Moments_Correct <- list(mu = c(0.014324566366636, -0.05, -0.014324566366636, 0.05
  ), sigma = structure(c(0.00026559040132714, -7.69330590577306e-06, 
                         0.000201032692767087, 0.000311890348241539, -7.69330590577306e-06, 
                         0.000535625726609157, -8.0756901128069e-06, 3.03012660915727e-05, 
                         0.000201032692767087, -8.0756901128069e-06, 0.000285105069674851, 
                         0.000422710091572661, 0.000311890348241539, 3.03012660915727e-05, 
                         0.000422710091572661, 0.00103173278354346), .Dim = c(4L, 4L), .Dimnames = list(
                           c("Convertible Arbitrage", "CTA Global", "Distressed Securities", 
                             "Emerging Markets"), c("Convertible Arbitrage", "CTA Global", 
                                                    "Distressed Securities", "Emerging Markets"))))
  expect_equal(AC_Moments, AC_Moments_Correct, tolerance = 1e-6)
  
})

test_that("ranking.moments works with meucci moments", {
  
  meucci_Moments <- ranking.moments(edhec[,1:4],n=3, method  = "meucci")
  
  
  meucci_Moments_correct <- list(mu = structure(c(0.00609182188064797, 0.0042195977, 0.00609182249537003, 
                                                  0.00609182159045832), .Dim = c(4L, 1L), .Dimnames = list(c("Convertible Arbitrage", 
                                                                                                             "CTA Global", "Distressed Securities", "Emerging Markets"), NULL)), 
                                 sigma = structure(c(0.000236659248371567, -9.89505966136701e-06, 
                                                     0.000185751376554954, 0.00028643894516584, -9.89505966136701e-06, 
                                                     0.000536075198698322, -1.38191585572314e-05, 1.38016556950264e-05, 
                                                     0.000185751376554954, -1.38191585572314e-05, 0.000283631324209191, 
                                                     0.000424993926887838, 0.00028643894516584, 1.38016556950264e-05, 
                                                     0.000424993926887838, 0.00104852997725043), .Dim = c(4L, 
                                                                                                          4L), .Dimnames = list(c("Convertible Arbitrage", "CTA Global", 
                                                                                                                                  "Distressed Securities", "Emerging Markets"), c("Convertible Arbitrage", 
                                                                                                                                                                                  "CTA Global", "Distressed Securities", "Emerging Markets"
                                                                                                                                  ))))
  
  expect_equal(meucci_Moments, meucci_Moments_correct, tolerance = 1e-6)
  
  
})
