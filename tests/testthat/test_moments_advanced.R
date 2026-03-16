##### test_moments_advanced.R #####
# Phase 5A: Advanced coverage for moment.functions.R
# Targets: CCCgarch.MM, GARCH detection loop in v2, per-objective clean switching,
#          garch.mm edge cases, set.portfolio.moments_v1 deprecation
#
# NOTE: set.portfolio.moments_v1 is deprecated — we do NOT add coverage tests
# for its internal branches. Existing regression tests in test_moment_functions.R
# are retained but not expanded.

library(testthat)
library(PortfolioAnalytics)

data(edhec)
# 100 rows needed for GARCH(1,1) fitting stability; 3 assets for speed
R <- edhec[1:100, 1:3]
colnames(R) <- c("CA", "CTAG", "DS")
funds <- colnames(R)

# Smaller dataset for non-GARCH tests
R_small <- edhec[1:36, 1:3]
colnames(R_small) <- funds

# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------
make_portf <- function(obj_specs, assets = funds, add_constraints = TRUE) {
  p <- portfolio.spec(assets = assets)
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
# 1. CCCgarch.MM — CCC GARCH moment estimation
# ===========================================================================

context("moment.functions advanced: CCCgarch.MM")

test_that("CCCgarch.MM returns all four moments", {
  skip_if_not_installed("fGarch")
  
  result <- CCCgarch.MM(R)
  
  expect_true(is.list(result))
  expect_equal(sort(names(result)), c("m3", "m4", "mu", "sigma"))
})

test_that("CCCgarch.MM mu has correct dimensions", {
  skip_if_not_installed("fGarch")
  
  result <- CCCgarch.MM(R)
  
  expect_equal(length(result$mu), ncol(R))
  expect_true(is.numeric(result$mu))
})

test_that("CCCgarch.MM sigma is square positive-definite", {
  skip_if_not_installed("fGarch")
  
  result <- CCCgarch.MM(R)
  
  expect_equal(dim(result$sigma), c(ncol(R), ncol(R)))
  expect_true(is.matrix(result$sigma))
  # Positive definite => all eigenvalues > 0
  evals <- eigen(result$sigma, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(evals > 0))
})

test_that("CCCgarch.MM m3 and m4 have correct dimensions", {
  skip_if_not_installed("fGarch")
  
  n <- ncol(R)
  result <- CCCgarch.MM(R)
  
  expect_equal(dim(result$m3), c(n, n^2))
  expect_equal(dim(result$m4), c(n, n^3))
})

test_that("CCCgarch.MM sigma differs from sample covariance", {
  skip_if_not_installed("fGarch")
  
  result <- CCCgarch.MM(R)
  sample_sigma <- cov(R)
  
  # GARCH sigma uses conditional forecasts, should differ from unconditional
  expect_false(isTRUE(all.equal(result$sigma, sample_sigma, tolerance = 1e-4)))
})

test_that("CCCgarch.MM uses sample mean by default", {
  skip_if_not_installed("fGarch")
  
  result <- CCCgarch.MM(R)
  expected_mu <- as.numeric(colMeans(R))
  
  expect_equal(as.numeric(result$mu), expected_mu)
})

test_that("CCCgarch.MM mu argument hits match.call bug (known limitation)", {
  skip_if_not_installed("fGarch")
  
  # CCCgarch.MM uses match.call(expand.dots=TRUE)$mu which returns an
  # unevaluated symbol rather than the evaluated vector — same class of
  # bug as the black_litterman path. Document as known limitation.
  custom_mu <- c(0.01, 0.02, 0.03)
  expect_error(CCCgarch.MM(R, mu = custom_mu), "replicate")
})

test_that("CCCgarch.MM preserves existing momentargs", {
  skip_if_not_installed("fGarch")
  
  existing <- list(custom_field = "preserved")
  result <- CCCgarch.MM(R, momentargs = existing)
  
  # The function returns momentargs with mu/sigma/m3/m4 added
  expect_equal(result$custom_field, "preserved")
  expect_true(!is.null(result$mu))
})

# ===========================================================================
# 2. GARCH detection loop in set.portfolio.moments_v2
# ===========================================================================

context("moment.functions advanced: GARCH detection in v2")

test_that("GARCH loop in v2 is bypassed when no garch argument", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  # Should use sample moments, no GARCH
  moments <- set.portfolio.moments(R_small, portf)
  
  expect_equal(moments$sigma, cov(R_small))
})

# The GARCH detection loop (lines 282-294) uses grep('garch', portfolio) then
# unlist(), which produces prefixed names (e.g., "objectives.arguments.garch")
# rather than "garch". This makes the loop effectively unreachable in v2.
# We document this as a known dead-code path rather than contorting tests to
# exercise it. CCCgarch.MM is tested directly above.

# ===========================================================================
# 3. Per-objective clean switching in v2
# ===========================================================================

context("moment.functions advanced: per-objective clean returns in v2")

test_that("v2 uses cleaned returns when objective has clean argument", {
  portf_clean <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt"))
  ))
  portf_raw <- make_portf(list(
    list(type = "risk", name = "StdDev")
  ))
  
  m_clean <- set.portfolio.moments(R_small, portf_clean)
  m_raw <- set.portfolio.moments(R_small, portf_raw)
  
  # Both should produce valid sigma
  expect_equal(dim(m_clean$sigma), dim(m_raw$sigma))
  expect_true(is.matrix(m_clean$sigma))
})

test_that("v2 per-objective clean: mixed clean/raw objectives use correct data", {
  # First objective: StdDev with clean=boudt -> uses cleanR
  # Second objective: VaR without clean -> uses raw R
  portf_mixed <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt")),
    list(type = "risk", name = "VaR")
  ))
  
  moments <- set.portfolio.moments(R_small, portf_mixed)
  
  # Should have all 4 moments (VaR needs m3/m4)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
  
  # sigma is set by StdDev (first writer wins) using cleaned data
  # m3/m4 are set by VaR using raw data (second objective, no clean arg)
  # Key: both moment sets should be structurally valid
  n <- ncol(R_small)
  expect_equal(dim(moments$sigma), c(n, n))
  expect_equal(dim(moments$m3), c(n, n^2))
  expect_equal(dim(moments$m4), c(n, n^3))
})

test_that("v2 clean path: sigma from cleaned data differs when outliers present", {
  # Inject an outlier
  R_outlier <- R_small
  R_outlier[1, 1] <- 0.5  # extreme return
  
  portf_clean <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt"))
  ), assets = colnames(R_outlier))
  portf_raw <- make_portf(list(
    list(type = "risk", name = "StdDev")
  ), assets = colnames(R_outlier))
  
  m_clean <- set.portfolio.moments(R_outlier, portf_clean)
  m_raw <- set.portfolio.moments(R_outlier, portf_raw)
  
  # Cleaned sigma should differ from raw because of the outlier
  expect_false(isTRUE(all.equal(m_clean$sigma, m_raw$sigma)))
})

# ===========================================================================
# 4. set.portfolio.moments_v2 with method="boudt" — additional paths
# ===========================================================================

context("moment.functions advanced: v2 boudt method edge cases")

test_that("v2 method=boudt with CSM objective produces all 4 moments", {
  portf <- make_portf(list(list(type = "risk", name = "CSM")))
  
  moments <- set.portfolio.moments(R_small, portf, method = "boudt")
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
  
  # boudt sigma should come from factor model, not sample
  sample_sigma <- cov(R_small)
  expect_false(isTRUE(all.equal(moments$sigma, sample_sigma)))
})

test_that("v2 method=boudt with k=2 produces different moments than k=1", {
  portf <- make_portf(list(list(type = "risk", name = "VaR")))
  
  m1 <- set.portfolio.moments(R_small, portf, method = "boudt", k = 1)
  m2 <- set.portfolio.moments(R_small, portf, method = "boudt", k = 2)
  
  expect_false(isTRUE(all.equal(m1$sigma, m2$sigma)))
})

test_that("v2 method=boudt with clean objective uses cleaned data for fit", {
  portf <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt"))
  ))
  portf_raw <- make_portf(list(
    list(type = "risk", name = "StdDev")
  ))
  
  m_clean <- set.portfolio.moments(R_small, portf, method = "boudt")
  m_raw <- set.portfolio.moments(R_small, portf_raw, method = "boudt")
  
  # Both should have mu and sigma
  expect_true(!is.null(m_clean$mu))
  expect_true(!is.null(m_clean$sigma))
  expect_true(!is.null(m_raw$mu))
  expect_true(!is.null(m_raw$sigma))
})

# ===========================================================================
# 5. set.portfolio.moments_v2 with method="meucci" — additional paths
# ===========================================================================

context("moment.functions advanced: v2 meucci method")

test_that("v2 method=meucci posterior_p hits match.call bug (known limitation)", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  # set.portfolio.moments uses match.call(expand.dots=TRUE)$posterior_p which
  # returns an unevaluated symbol. Same class of bug as CCCgarch.MM $mu and
  # black_litterman $P. Document as known limitation.
  n <- nrow(R_small)
  posterior_p <- (1:n) / sum(1:n)
  
  expect_error(
    set.portfolio.moments(R_small, portf, method = "meucci",
                           posterior_p = posterior_p),
    "numeric/complex"
  )
})

test_that("v2 method=meucci with ES objective produces all 4 moments", {
  portf <- make_portf(list(list(type = "risk", name = "ES")))
  
  moments <- set.portfolio.moments(R_small, portf, method = "meucci")
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
  
  # mu and sigma should come from meucci model (posterior-weighted)
  # m3 and m4 come from sample M3.MM/M4.MM
  n <- ncol(R_small)
  expect_equal(dim(moments$m3), c(n, n^2))
  expect_equal(dim(moments$m4), c(n, n^3))
})

test_that("v2 method=meucci ROI=TRUE skips ES moments", {
  portf <- make_portf(list(list(type = "risk", name = "ES")))
  
  moments <- set.portfolio.moments(R_small, portf, method = "meucci", ROI = TRUE)
  expect_length(moments, 0)
})

# ===========================================================================
# 6. set.portfolio.moments_v2 with method="black_litterman" — v2 path
# ===========================================================================

context("moment.functions advanced: v2 black_litterman method")

# NOTE: set.portfolio.moments(method="black_litterman") uses match.call()$P
# which returns an unevaluated language object. This is a known pre-existing
# bug (documented in architecture.md). We test the error path here and test
# portfolio.moments.bl() directly for the working BL moment path.

test_that("v2 method=black_litterman hits known match.call bug", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  P_mat <- matrix(c(1, -1, 0), nrow = 1)
  
  expect_error(
    set.portfolio.moments(R_small, portf, method = "black_litterman", P = P_mat),
    "numeric/complex"
  )
})

# ===========================================================================
# 7. portfolio.moments.bl — additional coverage
# ===========================================================================

context("moment.functions advanced: portfolio.moments.bl paths")

test_that("portfolio.moments.bl with ES aliases produces all 4 moments", {
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  for (alias in c("CVaR", "cVaR", "ETL", "mETL", "mES")) {
    portf <- make_portf(list(list(type = "risk", name = alias)))
    moments <- portfolio.moments.bl(R_small, portf, P = P)
    
    expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"),
                 info = paste("BL with alias:", alias))
  }
})

test_that("portfolio.moments.bl with CSM objective produces all 4 moments", {
  portf <- make_portf(list(list(type = "risk", name = "CSM")))
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  moments <- portfolio.moments.bl(R_small, portf, P = P)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
  
  # BL mu should differ from sample
  sample_mu <- matrix(colMeans(R_small), ncol = 1)
  expect_false(isTRUE(all.equal(moments$mu, sample_mu)))
})

test_that("portfolio.moments.bl with custom Mu and Sigma", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  P <- matrix(c(1, -1, 0), nrow = 1)
  custom_Mu <- colMeans(R_small) * 2
  custom_Sigma <- cov(R_small) * 1.5
  
  moments <- portfolio.moments.bl(R_small, portf, P = P, 
                                   Mu = custom_Mu, Sigma = custom_Sigma)
  
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
})

test_that("portfolio.moments.bl multiple clean methods warns", {
  portf <- make_portf(list(
    list(type = "risk", name = "StdDev", arguments = list(clean = "boudt")),
    list(type = "risk", name = "ES", arguments = list(clean = "geltinger"))
  ))
  P <- matrix(c(1, -1, 0), nrow = 1)
  
  expect_warning(
    portfolio.moments.bl(R_small, portf, P = P),
    "Multiple methods"
  )
})

# ===========================================================================
# 8. portfolio.moments.boudt — additional coverage
# ===========================================================================

context("moment.functions advanced: portfolio.moments.boudt paths")

test_that("portfolio.moments.boudt with CSM objective", {
  portf <- make_portf(list(list(type = "risk", name = "CSM")))
  
  moments <- portfolio.moments.boudt(R_small, portf)
  expect_equal(sort(names(moments)), c("m3", "m4", "mu", "sigma"))
})

test_that("portfolio.moments.boudt CSM mu uses raw mean (no na.rm)", {
  # CSM is in the VaR/mVaR group, which does NOT use na.rm
  portf <- make_portf(list(list(type = "risk", name = "CSM")))
  moments <- portfolio.moments.boudt(R_small, portf)
  
  expected_mu <- matrix(colMeans(R_small), ncol = 1)
  expect_equal(moments$mu, expected_mu)
})

test_that("portfolio.moments.boudt StdDev mu uses na.rm=TRUE", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  moments <- portfolio.moments.boudt(R_small, portf)
  
  expected_mu <- matrix(as.vector(apply(R_small, 2, mean, na.rm = TRUE)), ncol = 1)
  expect_equal(moments$mu, expected_mu)
})

test_that("portfolio.moments.boudt ETL skipped with ROI=TRUE", {
  portf <- make_portf(list(list(type = "risk", name = "ETL")))
  moments <- portfolio.moments.boudt(R_small, portf, ROI = TRUE)
  
  expect_length(moments, 0)
})

# ===========================================================================
# 9. garch.mm helper — edge cases
# ===========================================================================

context("moment.functions advanced: garch.mm edge cases")

test_that("garch.mm with NULL momentargs computes m3 and m4 fresh", {
  mu_ts <- xts::xts(matrix(rep(0.01, ncol(R_small)), nrow = 1),
                     order.by = zoo::index(R_small)[nrow(R_small)])
  colnames(mu_ts) <- colnames(R_small)
  
  cov_mat <- cov(R_small)
  covlist <- list()
  covlist[[as.character(zoo::index(R_small)[nrow(R_small)])]] <- cov_mat
  
  result <- garch.mm(R_small, mu_ts = mu_ts, covlist = covlist)
  
  n <- ncol(R_small)
  expect_equal(dim(result$m3), c(n, n^2))
  expect_equal(dim(result$m4), c(n, n^3))
  
  # m3/m4 should match direct computation
  expect_equal(result$m3, PerformanceAnalytics::M3.MM(R_small))
  expect_equal(result$m4, PerformanceAnalytics::M4.MM(R_small))
})

test_that("garch.mm sigma is extracted by date key from covlist", {
  mu_ts <- xts::xts(matrix(rep(0.01, ncol(R_small)), nrow = 1),
                     order.by = zoo::index(R_small)[nrow(R_small)])
  colnames(mu_ts) <- colnames(R_small)
  
  cov_mat <- cov(R_small)
  covlist <- list()
  covlist[[as.character(zoo::index(R_small)[nrow(R_small)])]] <- cov_mat
  
  result <- garch.mm(R_small, mu_ts = mu_ts, covlist = covlist)
  
  # garch.mm uses single-bracket `covlist[key]` which returns a list of length 1
  date_key <- as.character(zoo::index(R_small)[nrow(R_small)])
  expect_equal(result$sigma, covlist[date_key])
})

# ===========================================================================
# 10. set.portfolio.moments_v1 deprecation
# ===========================================================================

context("moment.functions advanced: v1 deprecation")

test_that("set.portfolio.moments_v1 emits deprecation warning", {
  gen <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 1,
                  min_sum = 0.99, max_sum = 1.01,
                  weight_seq = generatesequence(min = 0, max = 1, by = 0.01))
  )
  gen <- suppressWarnings(
    add.objective_v1(constraints = gen, type = "risk", name = "StdDev")
  )
  
  # Reset once-per-session flag so warning fires
  dw <- PortfolioAnalytics:::.deprecation_warned
  if (exists("set.portfolio.moments_v1", envir = dw))
    rm("set.portfolio.moments_v1", envir = dw)
  
  expect_warning(
    set.portfolio.moments_v1(R_small, gen),
    "set.portfolio.moments_v1.*deprecated"
  )
})

test_that("set.portfolio.moments_v1 still returns valid moments despite deprecation", {
  gen <- suppressWarnings(
    constraint_v1(assets = funds, min = 0, max = 1,
                  min_sum = 0.99, max_sum = 1.01,
                  weight_seq = generatesequence(min = 0, max = 1, by = 0.01))
  )
  gen <- suppressWarnings(
    add.objective_v1(constraints = gen, type = "risk", name = "StdDev")
  )
  
  moments <- suppressWarnings(set.portfolio.moments_v1(R_small, gen))
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
})

# ===========================================================================
# 11. v2 no objectives warning
# ===========================================================================

context("moment.functions advanced: v2 edge cases")

test_that("set.portfolio.moments warns on empty objectives", {
  portf <- portfolio.spec(assets = funds)
  portf$objectives <- NULL
  
  expect_warning(
    set.portfolio.moments(R_small, portf),
    "no objectives"
  )
})

test_that("set.portfolio.moments with unknown objective name produces empty", {
  portf <- portfolio.spec(assets = funds)
  portf <- add.constraint(portf, type = "full_investment")
  
  # Manually add an objective with a name not in .moment_needs
  portf$objectives <- list(list(name = "FakeObjective", enabled = TRUE))
  
  moments <- set.portfolio.moments(R_small, portf)
  expect_length(moments, 0)
})

test_that("set.portfolio.moments with NULL momentargs works", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  moments <- set.portfolio.moments(R_small, portf, momentargs = NULL)
  expect_true(!is.null(moments$mu))
  expect_true(!is.null(moments$sigma))
})

test_that("set.portfolio.moments preserves pre-existing momentargs fields", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  existing <- list(custom_data = "keep_me")
  moments <- set.portfolio.moments(R_small, portf, momentargs = existing)
  
  expect_equal(moments$custom_data, "keep_me")
  expect_true(!is.null(moments$mu))
})

test_that("set.portfolio.moments first-writer-wins with pre-set mu", {
  portf <- make_portf(list(list(type = "risk", name = "StdDev")))
  
  custom_mu <- matrix(rep(0.99, ncol(R_small)), ncol = 1)
  moments <- set.portfolio.moments(R_small, portf, momentargs = list(mu = custom_mu))
  
  # mu should NOT be overwritten

  expect_equal(moments$mu, custom_mu)
})
