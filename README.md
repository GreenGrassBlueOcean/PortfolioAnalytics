# PortfolioAnalytics (GreenGrassBlueOcean Fork)

[![R-CMD-check](https://github.com/GreenGrassBlueOcean/PortfolioAnalytics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GreenGrassBlueOcean/PortfolioAnalytics/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/GreenGrassBlueOcean/PortfolioAnalytics/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/GreenGrassBlueOcean/PortfolioAnalytics/actions/workflows/test-coverage.yaml)

Portfolio optimization in R with support for complex constraints, multiple objectives, and multiple solver backends.

This is a modernized fork of [braverock/PortfolioAnalytics](https://github.com/braverock/PortfolioAnalytics), adding production-grade error handling, modular solver architecture, comprehensive testing, and modern CI/CD infrastructure — while preserving full backward compatibility with existing code.

## Installation

```r
# Install from GitHub
remotes::install_github("GreenGrassBlueOcean/PortfolioAnalytics")
```

## What This Fork Adds

This fork builds on top of braverock's PortfolioAnalytics (which itself includes CVXR solvers, CSM/EQS risk measures, and robust covariance estimators) and adds 14 architectural improvements focused on **reliability**, **modularity**, and **testability**.

### Modular Solver Registry

The monolithic 600+ line solver dispatch chain has been replaced with a clean registry pattern. Each solver lives in its own file (`solver_deoptim.R`, `solver_roi.R`, `solver_pso.R`, `solver_gensa.R`, `solver_random.R`, `solver_cvxr.R`) with a uniform interface, making the code easier to understand and maintain.

Users can register custom solvers without modifying package internals:

```r
register_solver("my_solver", function(R, portfolio, constraints, moments, penalty,
                                       N, call, trace, search_size, rp,
                                       message = FALSE, ...) {
  # your solver logic here
  list(weights = w, objective_measures = list(), opt_values = list(), out = 0, call = call)
})

opt <- optimize.portfolio(R, portfolio, optimize_method = "my_solver")
```

### Structured Error Handling

Failed optimizations return typed `optimization_failure` S3 objects instead of cryptic error strings. Downstream code can test results with `is.optimization_failure(result)` rather than guessing about failure modes.

### Pre- and Post-Optimization Validation

- **`validate_portfolio()`** catches 14 classes of input errors (missing assets, incompatible constraints, infeasible bounds, etc.) before the solver runs
- **`check_portfolio_feasibility()`** checks the solution against all constraints after optimization, reporting per-constraint status (binding, inactive, or violated) with computed slack values

### Deterministic Constraint Repair

`project_weights()` uses Dykstra's alternating projection algorithm to find the nearest feasible portfolio in the intersection of box, weight-sum, and group constraints. This replaces the stochastic `rp_transform()` approach for convex constraint sets, with automatic fallback for non-convex constraints.

### Adaptive Penalty Calibration

`calibrate_penalty()` auto-scales the constraint violation penalty relative to the objective function magnitude. The default `penalty = "auto"` in `optimize.portfolio()` runs a 20-portfolio pilot to set the penalty, eliminating the need to hand-tune magic constants.

### Warm-Start Rebalancing

Stochastic solvers (DEoptim, GenSA, PSO) accept a `warm_start` argument seeded from the previous optimization window. In `optimize.portfolio.rebalancing()`, setting `warm_start = TRUE` passes optimal weights forward across windows, improving convergence speed for sequential rebalancing workflows.

### Reentrant Trace Storage

Trace accumulation uses a per-call local environment (`storage_env`) instead of a global `.storage` object, making concurrent or nested optimizations safe.

### Table-Driven Moment Functions

The combinatorial switch cascade in `set.portfolio.moments()` has been replaced with lookup tables (`.moment_needs`, `.moment_provider`), making it straightforward to add new risk measures or estimation methods.

### Deprecation System

Legacy v1 API functions (`constraint()`, `optimize.portfolio_v1()`, etc.) issue structured deprecation warnings. Hot-path functions like `constrained_objective_v1()` use `deprecate_once()` to warn once per session instead of flooding the console during optimization.

## What's Included from braverock

All features from [braverock/PortfolioAnalytics](https://github.com/braverock/PortfolioAnalytics) are included:

- **CVXR solver backend** supporting 10+ convex solvers (OSQP, SCS, ECOS, GLPK, MOSEK, GUROBI, etc.)
- **CSM and EQS risk measures** with efficient frontier support
- **Robust covariance estimators** (MM, Rocke, MCD, TSGS) via `custom.covRob.R`
- **Extended efficient frontiers** for mean-CSM and mean-EQS
- **Backtest plotting** and multi-frontier comparison utilities

## Supported Solvers

| Solver | Type | Method |
|--------|------|--------|
| ROI (quadprog, glpk, symphony) | Deterministic | `"ROI"` |
| CVXR (OSQP, SCS, ECOS, ...) | Deterministic | `"CVXR"` |
| DEoptim | Stochastic (differential evolution) | `"DEoptim"` |
| GenSA | Stochastic (simulated annealing) | `"GenSA"` |
| pso | Stochastic (particle swarm) | `"pso"` |
| Random portfolios | Stochastic (sampling) | `"random"` |

## Testing

The package includes 50 test files with 1,088+ passing assertions:

```r
devtools::test()
```

`R CMD check --as-cran` passes with 0 errors and 0 warnings.

## Quick Example

```r
library(PortfolioAnalytics)
data(edhec)
R <- edhec[, 1:6]

# Specify portfolio
port <- portfolio.spec(assets = colnames(R))
port <- add.constraint(port, type = "full_investment")
port <- add.constraint(port, type = "long_only")
port <- add.objective(port, type = "risk", name = "StdDev")
port <- add.objective(port, type = "return", name = "mean")

# Optimize
opt <- optimize.portfolio(R, port, optimize_method = "ROI", trace = TRUE)
print(opt)
chart.RiskReward(opt)

# Efficient frontier
ef <- create.EfficientFrontier(R, port, type = "mean-StdDev")
chart.EfficientFrontier(ef, match.col = "StdDev")
```

## Links

- **Upstream**: [braverock/PortfolioAnalytics](https://github.com/braverock/PortfolioAnalytics)
- **Original**: [R-Finance/PortfolioAnalytics](https://github.com/R-Finance/PortfolioAnalytics) (no longer actively developed)
- **Architecture docs**: See `architecture.md` in this repository

## License

GPL-3
