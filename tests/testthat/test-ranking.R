test_that("ranking.moments works with AC moments", {
  
  data("edhec")
  AC_Moments <- ranking.moments(edhec[,1:4], n=3, method = "ac")
  
  # Verify structure
  expect_true(is.list(AC_Moments))
  expect_true("mu" %in% names(AC_Moments))
  expect_true("sigma" %in% names(AC_Moments))
  expect_equal(length(AC_Moments$mu), 4)
  expect_equal(dim(AC_Moments$sigma), c(4, 4))
  
  # Verify sigma is positive semi-definite (eigenvalues >= 0)
  expect_true(all(eigen(AC_Moments$sigma, only.values = TRUE)$values >= -1e-10))
  
  # Snapshot current values (tolerance guards against platform differences)
  AC_Moments_Expected <- list(
    mu = c(-0.05, 0.014324566366636, 0.05, -0.014324566366636),
    sigma = structure(c(0.000280971684744495, -2.63667796530927e-06, 
      0.000227759134601898, 0.000345362020080415, -2.63667796530927e-06, 
      0.000519299456262565, 9.31619640936933e-07, 4.25797091963159e-05, 
      0.000227759134601898, 9.31619640936933e-07, 0.000329229000420777, 
      0.000468448795408855, 0.000345362020080415, 4.25797091963159e-05, 
      0.000468448795408855, 0.00106992239609145), .Dim = c(4L, 4L), 
      .Dimnames = list(
        c("Convertible Arbitrage", "CTA Global", "Distressed Securities", "Emerging Markets"),
        c("Convertible Arbitrage", "CTA Global", "Distressed Securities", "Emerging Markets"))))
  expect_equal(AC_Moments, AC_Moments_Expected, tolerance = 1e-6)
})

test_that("ranking.moments works with meucci moments", {
  
  meucci_Moments <- ranking.moments(edhec[,1:4], n=3, method = "meucci")
  
  # Verify structure
  expect_true(is.list(meucci_Moments))
  expect_true("mu" %in% names(meucci_Moments))
  expect_true("sigma" %in% names(meucci_Moments))
  expect_equal(nrow(meucci_Moments$mu), 4)
  expect_equal(dim(meucci_Moments$sigma), c(4, 4))
  
  # Verify sigma is positive semi-definite
  expect_true(all(eigen(meucci_Moments$sigma, only.values = TRUE)$values >= -1e-10))
  
  meucci_Moments_expected <- list(
    mu = structure(c(0.00518189143180863, 0.00518182018224983, 
      0.00607320091511104, 0.00518191257124293), .Dim = c(4L, 1L), 
      .Dimnames = list(c("Convertible Arbitrage", "CTA Global", 
        "Distressed Securities", "Emerging Markets"), NULL)),
    sigma = structure(c(0.000310186201613382, -1.12719895775411e-05, 
      0.000253724250106385, 0.000386460741564264, -1.12719895775411e-05, 
      0.000525866132624827, -1.26991866247182e-05, 1.51806336016699e-05, 
      0.000253724250106385, -1.26991866247182e-05, 0.000360658631921941, 
      0.000522354791970309, 0.000386460741564264, 1.51806336016699e-05, 
      0.000522354791970309, 0.00116639233374226), .Dim = c(4L, 4L), 
      .Dimnames = list(
        c("Convertible Arbitrage", "CTA Global", "Distressed Securities", "Emerging Markets"),
        c("Convertible Arbitrage", "CTA Global", "Distressed Securities", "Emerging Markets"))))
  expect_equal(meucci_Moments, meucci_Moments_expected, tolerance = 1e-4)
})
