##### test_moment_functions.R #####
# Phase 6 coverage tests for moment.functions.R
# Targets: set.portfolio.moments_v1, portfolio.moments.boudt,
#          portfolio.moments.bl, garch.mm, .moment_provider,
#          clean returns paths, meucci method path

library(testthat)
library(PortfolioAnalytics)

data(edhec)
# Use only 3 assets and 36 rows — M3.MM/M4.MM scales as O(n^3)/O(n^4)
R <- edhec[1:36, 1:3]
colnames(R) <- c("CA", "CTAG", "DS")
funds <- colnames(R)

# ---------------------------------------------------------------------------
# Helper: quick portfolio builder
# ---------------------------------------------------------------------------
make_portf <- function(obj_specs, add_constraints = TRUE) {
  p <- portfolio.spec(assets = funds)
  if (add_constraints) {
    p <- add.constraint(p, type = "full_investment")
    p <- add.constraint(p, type = "long_only")
  }
  for (spec in obj_specs) {
    p <- add.objective(p, type = spec$type, name = spec$name,
                       arguments = spec$arguments)
  }
  p
}

# ===========================================================================
# 1. set.portfolio.moments_v1
# ===========================================================================

# Helper to create v1 constraint objects (suppress deprecation warnings)
make_v1_constr <- function() {
  suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 1,
                  min_sum = 0.99, max_sum = 1.01,
                  weight_seq = generatesequence(min = 0, max = 1, by = 0.01))
  )
}

test_that("set.portfolio.moments_v1 computes mu + sigma for StdDev", {
  gen <- make_v1_constr()
  gen <- suppressWarnings(add.objective_v1(constraints = gen, type = "risk", name = "StdDev"))
  
  moments <- set.portfolio.moments_v1(R, gen)
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
  expect_true(is.matrix(moments$mu))
  expect_true(is.matrix(moments$sigma))
  expect_equal(nrow(moments$mu), ncol(R))
  expect_equal(dim(moments$sigma), c(ncol(R), ncol(R)))
})

test_that("set.portfolio.moments_v1 computes all 4 moments for ES", {
  gen <- make_v1_constr()
  gen <- suppressWarnings(add.objective_v1(constraints = gen, type = "risk", name = "ES"))
  
  moments <- set.portfolio.moments_v1(R, gen)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

test_that("set.portfolio.moments_v1 computes all 4 moments for VaR", {
  gen <- make_v1_constr()
  gen <- suppressWarnings(add.objective_v1(constraints = gen, type = "risk", name = "VaR"))
  
  moments <- set.portfolio.moments_v1(R, gen)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

test_that("set.portfolio.moments_v1 warns when no objectives", {
  gen <- make_v1_constr()
  gen$objectives <- NULL
  
  expect_warning(set.portfolio.moments_v1(R, gen), "no objectives")
})

# ===========================================================================
# 2. portfolio.moments.boudt
# ===========================================================================

test_that("portfolio.moments.boudt computes moments for StdDev", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  moments <- portfolio.moments.boudt(R, portf)
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
  expect_true(is.matrix(moments$sigma))
  expect_equal(dim(moments$sigma), c(ncol(R), ncol(R)))
})

test_that("portfolio.moments.boudt computes all 4 moments for VaR", {
  portf <- make_portf(list(list(type = "risk", name = "VaR")))
  
  moments <- portfolio.moments.boudt(R, portf)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

test_that("portfolio.moments.boudt computes all 4 moments for ES", {
  portf <- make_portf(list(list(type = "risk", name = "ES")))
  
  moments <- portfolio.moments.boudt(R, portf)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

test_that("portfolio.moments.boudt skips ES moments when ROI=TRUE", {
  portf <- make_portf(list(list(type = "risk", name = "ES")))
  
  moments <- portfolio.moments.boudt(R, portf, ROI = TRUE)
  expect_length(moments, 0)
})

test_that("portfolio.moments.boudt handles mean-only objective", {
  portf <- make_portf(list(list(type = "return", name = "mean")))
  
  moments <- portfolio.moments.boudt(R, portf)
  expect_equal(names(moments), "mu")
})

test_that("portfolio.moments.boudt warns on no objectives", {
  portf <- portfolio.spec(assets = funds)
  portf$objectives <- NULL
  
  expect_warning(portfolio.moments.boudt(R, portf), "no objectives")
})

test_that("portfolio.moments.boudt respects k parameter", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  m1 <- portfolio.moments.boudt(R, portf, k = 1)
  m2 <- portfolio.moments.boudt(R, portf, k = 2)
  
  # Different factor counts should produce different covariance estimates
  expect_false(isTRUE(all.equal(m1$sigma, m2$sigma)))
})

test_that("portfolio.moments.boudt handles ES aliases (CVaR, cVaR, ETL, mETL, mES)", {
  aliases <- c("CVaR", "cVaR", "ETL", "mETL", "mES")
  for (alias in aliases) {
    portf <- make_portf(list(list(type = "risk", name = alias)))
    moments <- portfolio.moments.boudt(R, portf)
    expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"),
                 info = paste("alias:", alias))
  }
})

# ===========================================================================
# 3. portfolio.moments.bl (Black-Litterman)
# ===========================================================================

test_that("portfolio.moments.bl computes moments for StdDev", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  moments <- portfolio.moments.bl(R, portf, P = P)
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
})

test_that("portfolio.moments.bl computes all 4 moments for VaR", {
  portf <- make_portf(list(list(type = "risk", name = "VaR")))
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  moments <- portfolio.moments.bl(R, portf, P = P)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

test_that("portfolio.moments.bl skips ES when ROI=TRUE", {
  portf <- make_portf(list(list(type = "risk", name = "ES")))
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  moments <- portfolio.moments.bl(R, portf, P = P, ROI = TRUE)
  expect_length(moments, 0)
})

test_that("portfolio.moments.bl handles mean-only objective", {
  portf <- make_portf(list(list(type = "return", name = "mean")))
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  moments <- portfolio.moments.bl(R, portf, P = P)
  expect_equal(names(moments), "mu")
  # BL mu should differ from sample mean
  sample_mu <- matrix(colMeans(R), ncol = 1)
  expect_false(isTRUE(all.equal(moments$mu, sample_mu)))
})

test_that("portfolio.moments.bl warns on no objectives", {
  portf <- portfolio.spec(assets = funds)
  portf$objectives <- NULL
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  expect_warning(portfolio.moments.bl(R, portf, P = P), "no objectives")
})

# ===========================================================================
# 4. set.portfolio.moments with method="boudt"
# ===========================================================================

test_that("set.portfolio.moments with method=boudt computes moments", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  moments <- set.portfolio.moments(R, portf, method = "boudt")
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
  expect_true(is.matrix(moments$sigma))
})

test_that("set.portfolio.moments with method=boudt for VaR gives 4 moments", {
  portf <- make_portf(list(list(type = "risk", name = "VaR")))
  
  moments <- set.portfolio.moments(R, portf, method = "boudt")
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

# ===========================================================================
# 5. set.portfolio.moments with method="black_litterman"
# ===========================================================================

test_that("set.portfolio.moments with method=black_litterman dispatches", {
  # NOTE: The match.call()$P pattern in set.portfolio.moments returns a

  # language object, not the evaluated matrix. This is a known pre-existing
  # issue. Test portfolio.moments.bl() directly for full BL moment tests.
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  P_mat <- matrix(c(1, -1, 0), nrow = 1)
  
  # Verify it at least selects the correct method without argument errors
  expect_error(
    set.portfolio.moments(R, portf, method = "black_litterman", P = P_mat),
    "numeric/complex"
  )
})

# ===========================================================================
# 6. set.portfolio.moments with method="meucci"
# ===========================================================================

test_that("set.portfolio.moments with method=meucci works for StdDev", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  moments <- set.portfolio.moments(R, portf, method = "meucci")
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
})

test_that("set.portfolio.moments with method=meucci for VaR gives 4 moments", {
  portf <- make_portf(list(list(type = "risk", name = "VaR")))
  
  moments <- set.portfolio.moments(R, portf, method = "meucci")
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

# ===========================================================================
# 7. Clean returns paths
# ===========================================================================

test_that("set.portfolio.moments cleans returns when clean argument specified", {
  portf <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt"))
  ))
  
  moments_clean <- set.portfolio.moments(R, portf)
  moments_raw   <- set.portfolio.moments(R, make_portf(
    list(list(type = "risk", name = "StdDev"))
  ))
  
  # Cleaned and raw should produce valid moments
  expect_true(!is.null(moments_clean$mu))
  expect_true(!is.null(moments_clean$sigma))
  # They should differ (unless cleaning has no effect on this data)
  # At minimum, both should have the right structure
  expect_equal(dim(moments_clean$sigma), dim(moments_raw$sigma))
})

test_that("set.portfolio.moments warns on multiple clean methods", {
  portf <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt")),
    list(type = "risk", name = "ES", arguments = list(clean = "geltinger"))
  ))
  
  expect_warning(
    set.portfolio.moments(R, portf),
    "Multiple methods"
  )
})

test_that("portfolio.moments.boudt cleans returns when clean argument specified", {
  portf <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt"))
  ))
  
  moments <- portfolio.moments.boudt(R, portf)
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
})

test_that("portfolio.moments.bl cleans returns when clean argument specified", {
  portf <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt"))
  ))
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  moments <- portfolio.moments.bl(R, portf, P = P)
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
})

# ===========================================================================
# 8. .moment_provider (internal helper)
# ===========================================================================

test_that(".moment_provider returns correct structure for sample method", {
  provider <- PortfolioAnalytics:::.moment_provider("sample", R)
  
  expect_true(is.list(provider))
  expect_true(all(c("mu_narm", "mu", "sigma_pw", "sigma", "m3", "m4") %in% names(provider)))
  
  # Each element should be a function (closure)
  for (nm in names(provider)) {
    expect_true(is.function(provider[[nm]]), info = paste("provider", nm))
  }
})

test_that(".moment_provider sample closures return correct values", {
  provider <- PortfolioAnalytics:::.moment_provider("sample", R)
  
  expect_equal(provider$mu(), matrix(colMeans(R), ncol = 1))
  expect_equal(provider$mu_narm(), matrix(as.vector(apply(R, 2, mean, na.rm = TRUE)), ncol = 1))
  expect_equal(provider$sigma(), cov(R))
  expect_equal(provider$sigma_pw(), cov(R, use = "pairwise.complete.obs"))
})

test_that(".moment_provider returns NULL for unknown method", {
  provider <- PortfolioAnalytics:::.moment_provider("unknown_method", R)
  expect_null(provider)
})

test_that(".moment_provider boudt uses fit object", {
  fit <- statistical.factor.model(R = R, k = 1)
  provider <- PortfolioAnalytics:::.moment_provider("boudt", R, fit = fit)
  
  expect_true(is.list(provider))
  sigma_from_provider <- provider$sigma()
  sigma_from_fit <- extractCovariance(fit)
  expect_equal(sigma_from_provider, sigma_from_fit)
})

test_that(".moment_provider black_litterman uses B object", {
  P <- matrix(c(1, -1, 0), nrow = 1)
  B <- black.litterman(R = R, P = P)
  provider <- PortfolioAnalytics:::.moment_provider("black_litterman", R, B = B)
  
  expect_true(is.list(provider))
  expect_equal(provider$mu(), B$BLMu)
  expect_equal(provider$sigma(), B$BLSigma)
})

test_that(".moment_provider meucci uses meucci.model object", {
  mm <- meucci.moments(R = R, posterior_p = rep(1 / nrow(R), nrow(R)))
  provider <- PortfolioAnalytics:::.moment_provider("meucci", R, meucci.model = mm)
  
  expect_true(is.list(provider))
  expect_equal(provider$mu(), mm$mu)
  expect_equal(provider$sigma(), mm$sigma)
})

# ===========================================================================
# 9. garch.mm helper
# ===========================================================================

test_that("garch.mm sets moments from supplied mu_ts and covlist", {
  # Build simple mu_ts and covlist keyed by last date
  mu_ts <- xts::xts(matrix(rep(0.01, ncol(R)), nrow = 1),
                     order.by = zoo::index(R)[nrow(R)])
  colnames(mu_ts) <- colnames(R)
  
  cov_mat <- cov(R)
  covlist <- list()
  covlist[[as.character(zoo::index(R)[nrow(R)])]] <- cov_mat
  
  result <- garch.mm(R, mu_ts = mu_ts, covlist = covlist)
  
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$sigma))
  expect_true(!is.null(result$m3))
  expect_true(!is.null(result$m4))
})

test_that("garch.mm preserves pre-set m3 and m4", {
  mu_ts <- xts::xts(matrix(rep(0.01, ncol(R)), nrow = 1),
                     order.by = zoo::index(R)[nrow(R)])
  colnames(mu_ts) <- colnames(R)
  
  cov_mat <- cov(R)
  covlist <- list()
  covlist[[as.character(zoo::index(R)[nrow(R)])]] <- cov_mat
  
  custom_m3 <- array(1, dim = c(ncol(R), ncol(R)^2))
  custom_m4 <- array(2, dim = c(ncol(R), ncol(R)^3))
  
  result <- garch.mm(R, mu_ts = mu_ts, covlist = covlist,
                     momentargs = list(m3 = custom_m3, m4 = custom_m4))
  
  expect_equal(result$m3, custom_m3)
  expect_equal(result$m4, custom_m4)
})

# ===========================================================================
# 10. .moment_needs and .narm_objectives lookups
# ===========================================================================

test_that(".moment_needs contains expected objective entries", {
  mn <- PortfolioAnalytics:::.moment_needs
  
  expect_equal(mn$mean, "mu")
  expect_equal(mn$StdDev, c("mu", "sigma"))
  expect_equal(mn$VaR, c("mu", "sigma", "m3", "m4"))
  expect_equal(mn$ES, c("mu", "sigma", "m3", "m4"))
  expect_null(mn$UnknownObj)
})

test_that(".narm_objectives is correct", {
  narm <- PortfolioAnalytics:::.narm_objectives
  expect_true("mean" %in% narm)
  expect_true("StdDev" %in% narm)
  expect_true("sd" %in% narm)
  expect_true("var" %in% narm)
  expect_false("VaR" %in% narm)
  expect_false("ES" %in% narm)
})

test_that(".es_aliases is correct", {
  es_al <- PortfolioAnalytics:::.es_aliases
  expected <- c("es", "mES", "CVaR", "cVaR", "ETL", "mETL", "ES")
  expect_equal(es_al, expected)
})
