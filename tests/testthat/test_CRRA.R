
require(testthat)
require(PortfolioAnalytics)

context("test CRRA objective")

data(edhec)

ret <-edhec[,1:4]

test_that("crra.moments satisfies testcase",{
  test_Moments <- crra.moments(ret)
  correct_Moments <- list(mu = c(`Convertible Arbitrage` = 0.00640855263157895, `CTA Global` = 0.00648947368421053, 
                                 `Distressed Securities` = 0.00795328947368421, `Emerging Markets` = 0.00824605263157895 )
                          , sigma = structure(c(0.000401897740937609, -3.43584524224468e-05, 
                                                 0.000262344309428372, 0.000428975298884629, -3.43584524224468e-05, 
                                                 0.000631562140118508, -3.71291042174974e-05, -1.01445451376786e-05, 
                                                 0.000262344309428372, -3.71291042174974e-05, 0.000336645816922273, 
                                                 0.000568065344196584, 0.000428975298884629, -1.01445451376786e-05, 
                                                 0.000568065344196584, 0.00148775561345417)
                                              , .Dim = c(4L, 4L), .Dimnames = list(c("Convertible Arbitrage", "CTA Global", "Distressed Securities", "Emerging Markets")
                             , c("Convertible Arbitrage", "CTA Global", "Distressed Securities", "Emerging Markets")))
                          , m3 = structure(c(-2.14091952528606e-05, 2.48678177806531e-06, -1.42008427046781e-05, -2.41695213506035e-05, 
                                              2.48678177806531e-06, -2.49587465210399e-06, 3.65577443361824e-06, 
                                              6.78509861575576e-06, -1.42008427046781e-05, 3.65577443361824e-06, 
                                              -1.03126121807792e-05, -1.78089762977464e-05, -2.41695213506035e-05, 
                                              6.78509861575576e-06, -1.78089762977464e-05, -3.01739702837535e-05, 
                                              2.48678177806531e-06, -2.49587465210399e-06, 3.65577443361824e-06, 
                                              6.78509861575576e-06, -2.49587465210399e-06, 2.11332434462021e-06, 
                                              -3.33794704004319e-06, -5.31129998442739e-06, 3.65577443361824e-06, 
                                              -3.33794704004319e-06, 5.71745202972372e-06, 1.1799779887479e-05, 
                                              6.78509861575576e-06, -5.31129998442739e-06, 1.1799779887479e-05, 
                                              2.4147639059575e-05, -1.42008427046781e-05, 3.65577443361824e-06, 
                                              -1.03126121807792e-05, -1.78089762977464e-05, 3.65577443361824e-06, 
                                              -3.33794704004319e-06, 5.71745202972372e-06, 1.1799779887479e-05, 
                                              -1.03126121807792e-05, 5.71745202972372e-06, -1.02415886073561e-05, 
                                              -1.93959484469777e-05, -1.78089762977464e-05, 1.1799779887479e-05, 
                                              -1.93959484469777e-05, -3.69527529535852e-05, -2.41695213506035e-05, 
                                              6.78509861575576e-06, -1.78089762977464e-05, -3.01739702837535e-05, 
                                              6.78509861575576e-06, -5.31129998442739e-06, 1.1799779887479e-05, 
                                              2.4147639059575e-05, -1.78089762977464e-05, 1.1799779887479e-05, 
                                              -1.93959484469777e-05, -3.69527529535852e-05, -3.01739702837535e-05, 
                                              2.4147639059575e-05, -3.69527529535852e-05, -7.14511036943752e-05), .Dim = c(4L, 16L))
                          , m4 = structure(c(3.05706984796178e-06, -3.77019303702818e-07, 1.91429716938307e-06, 3.23569879132515e-06, 
                                            -3.77019303702818e-07, 2.28110745704175e-07, -2.9619022438666e-07, 
                                            -5.05641132985641e-07, 1.91429716938307e-06, -2.9619022438666e-07, 
                                            1.28589177455422e-06, 2.19624227723883e-06, 3.23569879132515e-06, 
                                            -5.05641132985641e-07, 2.19624227723883e-06, 3.84382863959623e-06, 
                                            -3.77019303702818e-07, 2.28110745704175e-07, -2.9619022438666e-07, 
                                            -5.05641132985641e-07, 2.28110745704175e-07, -7.11361240399873e-08, 
                                            2.12333436950948e-07, 3.81527882335961e-07, -2.9619022438666e-07, 
                                            2.12333436950948e-07, -2.97635357502199e-07, -5.56453700108169e-07, 
                                            -5.05641132985641e-07, 3.81527882335961e-07, -5.56453700108169e-07, 
                                            -1.06497671171445e-06, 1.91429716938307e-06, -2.9619022438666e-07, 
                                            1.28589177455422e-06, 2.19624227723883e-06, -2.9619022438666e-07, 
                                            2.12333436950948e-07, -2.97635357502199e-07, -5.56453700108169e-07, 
                                            1.28589177455422e-06, -2.97635357502199e-07, 9.85799936011211e-07, 
                                            1.73866271358716e-06, 2.19624227723883e-06, -5.56453700108169e-07, 
                                            1.73866271358716e-06, 3.15458723487811e-06, 3.23569879132515e-06, 
                                            -5.05641132985641e-07, 2.19624227723883e-06, 3.84382863959623e-06, 
                                            -5.05641132985641e-07, 3.81527882335961e-07, -5.56453700108169e-07, 
                                            -1.06497671171445e-06, 2.19624227723883e-06, -5.56453700108169e-07, 
                                            1.73866271358716e-06, 3.15458723487811e-06, 3.84382863959623e-06, 
                                            -1.06497671171445e-06, 3.15458723487811e-06, 5.86433476064901e-06, 
                                            -3.77019303702818e-07, 2.28110745704175e-07, -2.9619022438666e-07, 
                                            -5.05641132985641e-07, 2.28110745704175e-07, -7.11361240399873e-08, 
                                            2.12333436950948e-07, 3.81527882335961e-07, -2.9619022438666e-07, 
                                            2.12333436950948e-07, -2.97635357502199e-07, -5.56453700108169e-07, 
                                            -5.05641132985641e-07, 3.81527882335961e-07, -5.56453700108169e-07, 
                                            -1.06497671171445e-06, 2.28110745704175e-07, -7.11361240399873e-08, 
                                            2.12333436950948e-07, 3.81527882335961e-07, -7.11361240399873e-08, 
                                            1.1363077898662e-06, -1.59862140869611e-07, -2.45612440174597e-07, 
                                            2.12333436950948e-07, -1.59862140869611e-07, 3.6076760386263e-07, 
                                            6.89129715124477e-07, 3.81527882335961e-07, -2.45612440174597e-07, 
                                            6.89129715124477e-07, 1.66189585598489e-06, -2.9619022438666e-07, 
                                            2.12333436950948e-07, -2.97635357502199e-07, -5.56453700108169e-07, 
                                            2.12333436950948e-07, -1.59862140869611e-07, 3.6076760386263e-07, 
                                            6.89129715124477e-07, -2.97635357502199e-07, 3.6076760386263e-07, 
                                            -4.34337458513675e-07, -8.69758117508934e-07, -5.56453700108169e-07, 
                                            6.89129715124477e-07, -8.69758117508934e-07, -1.76089500083644e-06, 
                                            -5.05641132985641e-07, 3.81527882335961e-07, -5.56453700108169e-07, 
                                            -1.06497671171445e-06, 3.81527882335961e-07, -2.45612440174597e-07, 
                                            6.89129715124477e-07, 1.66189585598489e-06, -5.56453700108169e-07, 
                                            6.89129715124477e-07, -8.69758117508934e-07, -1.76089500083644e-06, 
                                            -1.06497671171445e-06, 1.66189585598489e-06, -1.76089500083644e-06, 
                                            -3.53835452130294e-06, 1.91429716938307e-06, -2.9619022438666e-07, 
                                            1.28589177455422e-06, 2.19624227723883e-06, -2.9619022438666e-07, 
                                            2.12333436950948e-07, -2.97635357502199e-07, -5.56453700108169e-07, 
                                            1.28589177455422e-06, -2.97635357502199e-07, 9.85799936011211e-07, 
                                            1.73866271358716e-06, 2.19624227723883e-06, -5.56453700108169e-07, 
                                            1.73866271358716e-06, 3.15458723487811e-06, -2.9619022438666e-07, 
                                            2.12333436950948e-07, -2.97635357502199e-07, -5.56453700108169e-07, 
                                            2.12333436950948e-07, -1.59862140869611e-07, 3.6076760386263e-07, 
                                            6.89129715124477e-07, -2.97635357502199e-07, 3.6076760386263e-07, 
                                            -4.34337458513675e-07, -8.69758117508934e-07, -5.56453700108169e-07, 
                                            6.89129715124477e-07, -8.69758117508934e-07, -1.76089500083644e-06, 
                                            1.28589177455422e-06, -2.97635357502199e-07, 9.85799936011211e-07, 
                                            1.73866271358716e-06, -2.97635357502199e-07, 3.6076760386263e-07, 
                                            -4.34337458513675e-07, -8.69758117508934e-07, 9.85799936011211e-07, 
                                            -4.34337458513675e-07, 1.05569674607867e-06, 1.96264421903677e-06, 
                                            1.73866271358716e-06, -8.69758117508934e-07, 1.96264421903677e-06, 
                                            3.84595514303485e-06, 2.19624227723883e-06, -5.56453700108169e-07, 
                                            1.73866271358716e-06, 3.15458723487811e-06, -5.56453700108169e-07, 
                                            6.89129715124477e-07, -8.69758117508934e-07, -1.76089500083644e-06, 
                                            1.73866271358716e-06, -8.69758117508934e-07, 1.96264421903677e-06, 
                                            3.84595514303485e-06, 3.15458723487811e-06, -1.76089500083644e-06, 
                                            3.84595514303485e-06, 7.88264543467183e-06, 3.23569879132515e-06, 
                                            -5.05641132985641e-07, 2.19624227723883e-06, 3.84382863959623e-06, 
                                            -5.05641132985641e-07, 3.81527882335961e-07, -5.56453700108169e-07, 
                                            -1.06497671171445e-06, 2.19624227723883e-06, -5.56453700108169e-07, 
                                            1.73866271358716e-06, 3.15458723487811e-06, 3.84382863959623e-06, 
                                            -1.06497671171445e-06, 3.15458723487811e-06, 5.86433476064901e-06, 
                                            -5.05641132985641e-07, 3.81527882335961e-07, -5.56453700108169e-07, 
                                            -1.06497671171445e-06, 3.81527882335961e-07, -2.45612440174597e-07, 
                                            6.89129715124477e-07, 1.66189585598489e-06, -5.56453700108169e-07, 
                                            6.89129715124477e-07, -8.69758117508934e-07, -1.76089500083644e-06, 
                                            -1.06497671171445e-06, 1.66189585598489e-06, -1.76089500083644e-06, 
                                            -3.53835452130294e-06, 2.19624227723883e-06, -5.56453700108169e-07, 
                                            1.73866271358716e-06, 3.15458723487811e-06, -5.56453700108169e-07, 
                                            6.89129715124477e-07, -8.69758117508934e-07, -1.76089500083644e-06, 
                                            1.73866271358716e-06, -8.69758117508934e-07, 1.96264421903677e-06, 
                                            3.84595514303485e-06, 3.15458723487811e-06, -1.76089500083644e-06, 
                                            3.84595514303485e-06, 7.88264543467183e-06, 3.84382863959623e-06, 
                                            -1.06497671171445e-06, 3.15458723487811e-06, 5.86433476064901e-06, 
                                            -1.06497671171445e-06, 1.66189585598489e-06, -1.76089500083644e-06, 
                                            -3.53835452130294e-06, 3.15458723487811e-06, -1.76089500083644e-06, 
                                            3.84595514303485e-06, 7.88264543467183e-06, 5.86433476064901e-06, 
                                            -3.53835452130294e-06, 7.88264543467183e-06, 1.76992198828617e-05
                                            ), .Dim = c(4L, 64L)))
  
  expect_equal(test_Moments, correct_Moments)
  
})

test_that("crra objective function satisfies testcase",{

Moments <- crra.moments(ret)
Equalweights <- rep(1/ncol(ret), ncol(ret))
CRRA_test <- CRRA(R = ret, weights = Equalweights, lambda = 5, sigma = Moments$sigma, m3 = Moments$m3, m4 = Moments$m4)

CRRA_expected <- structure(-0.000853543786499519, .Dim = c(1L, 1L))

expect_equal(CRRA_test,CRRA_expected )

})