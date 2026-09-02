# PortfolioAnalytics (GreenGrassBlueOcean Fork)

[![R-CMD-check](https://github.com/GreenGrassBlueOcean/PortfolioAnalytics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GreenGrassBlueOcean/PortfolioAnalytics/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/GreenGrassBlueOcean/PortfolioAnalytics/graph/badge.svg)](https://codecov.io/gh/GreenGrassBlueOcean/PortfolioAnalytics)

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

### Parallelism Changes

Parallel execution for DEoptim has been redesigned to be safe and CRAN-compliant:

- **Parallel is opt-in** (`parallel = FALSE` by default). Upstream defaults to `parallel = TRUE`, which causes immediate SOCK cluster failures on Windows when no `foreach` backend is registered. Pass `parallel = TRUE` explicitly to enable parallel DEoptim.
- **Self-contained cluster lifecycle** — When `parallel = TRUE`, `solve_deoptim()` creates a PSOCK cluster via base R's `parallel::makeCluster()`, registers it with `doSNOW`, and tears it down in `on.exit()` — guaranteeing cleanup even on error. Upstream uses `snow::makeSOCKcluster()` and only cleans up on the happy path.
- **No global `foreach` side effects** — `random_portfolios()` no longer calls `registerDoSEQ()` or uses `%dopar%`. All random portfolio generation (simplex, grid, sample) runs sequentially. Users who need to parallelize large portfolio generation (`permutations > 50,000`) should register a `foreach` backend before calling `optimize.portfolio()`.
- **Nested parallelism safe** — An outer user-level `foreach` cluster survives inner `solve_deoptim(parallel = TRUE)` calls. Each inner call creates and destroys its own cluster independently.
- **Bounded cluster size (`MaxCores`)** — `optimize.portfolio()` takes a `MaxCores` argument (default `15`). The cluster is sized `min(parallel::detectCores(), MaxCores)`, replacing a hard-coded `ifelse(nC <= 15, nC, 15)`. The default reproduces the previous sizing at every core count; lower it to leave cores free for other work.
- **Cluster size is reported** — with `message = TRUE`, `solve_deoptim()` logs `DEoptim parallel cluster: N worker(s) (detectCores=..., MaxCores=...)`. A cluster of the wrong size used to be completely invisible in the logs.
- **Silent serial fallback now warns** — `parallel = TRUE` only takes effect when `foreach` is *attached*, not merely installed. That combination previously ran the entire optimisation sequentially without a word, so a run just took N times longer with no indication why. It now warns.
- **Parallel and sequential runs agree** — `set.seed(n)` now gives the same weights whether or not `parallel = TRUE`. Loading `doSNOW` consumes one draw from the global RNG stream, and that load happens only on the parallel path, so a parallel run used to start the search one draw further along than a sequential one: different initial population, different mutation sequence, different optimum, from identical inputs. It was reproducible *within* a mode — two parallel runs were bit-identical — which is exactly what disguised it. `solve_deoptim()` and `optimize.portfolio_v1()` now snapshot `.Random.seed` before cluster setup and restore it afterwards, so the setup cost is unwound. Measured on a 9-fold walk-forward before the fix: Sharpe 0.4535 parallel vs 0.5778 sequential, with one weight differing by 0.287. (`snow` and `doSNOW` shift the stream; `foreach`, `iterators`, `doParallel` and `DEoptim` do not.)
- **Workers get the master's attached packages** — the setup call was `clusterEvalQ(rcl, lapply(names(sessionInfo()$otherPkgs), require, ...))`, which loaded nothing: `clusterEvalQ` evaluates on the *worker*, where nothing is attached, so `otherPkgs` was `NULL` there. That mattered because `constrained_objective()` special-cases a few measures by symbol (`StdDev`, `VaR`, `ES`, `mean`, `median`, `turnover`) but resolves everything else — `CRRA` included — with `match.fun(objective$name)`, a string lookup against the *caller's* frame. On a bare worker that name is unreachable. The package list is now computed in the master and shipped with `clusterCall()`.
- **A failed objective is named, not swallowed** — an objective that could not be evaluated was reported with `message()` and then used in arithmetic anyway, so the only visible symptom was `non-numeric argument to binary operator` raised several lines later. `constrained_objective()` now raises at the point of failure, naming the objective and flagging the bare-worker case. This changes no result: every branch multiplies the measure, so a `try-error` already killed the run one line on.
- **Bounded nesting (`MaxSubCores`)** — `optimize.portfolio.rebalancing()` takes a `MaxSubCores` argument (default `1`). Rebalance periods are already distributed with `%dopar%`, so letting each worker build its own cluster oversubscribes the machine by *outer × inner* workers. Previously the inner `optimize.portfolio()` calls were hard-wired to `parallel = FALSE` — safe, but with no way to use spare cores when there are few rebalance periods and many assets. `MaxSubCores = 1` reproduces that behaviour exactly (`parallel = (1 > 1)` is `FALSE`); values above 1 opt into bounded nesting, capping each inner cluster. Choose it so that `outer_workers * MaxSubCores` stays within the core count.

```r
# Allow each rebalance-period worker up to 2 cores for its inner optimization
optimize.portfolio.rebalancing(R, portfolio, optimize_method = "DEoptim",
                               rebalance_on = "quarters", training_period = 36,
                               MaxSubCores = 2)
```

**Known difference from upstream:** The DEoptim `strategy` parameter defaults to `2` (DE/rand/1/bin) in this fork, whereas upstream uses `6` (DE/current-to-p-best/1, JADE-style). Strategy 6 generally converges faster for portfolio problems. To match upstream behavior, pass `strategy = 6` explicitly:

```r
optimize.portfolio(R, portfolio, optimize_method = "DEoptim", strategy = 6)
```

### Deprecation System

Legacy v1 API functions (`constraint()`, `optimize.portfolio_v1()`, etc.) issue structured deprecation warnings. Hot-path functions like `constrained_objective_v1()` use `deprecate_once()` to warn once per session instead of flooding the console during optimization.

### Bug Fix: `match.call()` Without `eval.parent()` in `random_portfolios()`

The upstream `random_portfolios()` function uses `match.call(expand.dots=TRUE)$fev` and `match.call(expand.dots=TRUE)$normalize` to extract pass-through arguments from `...`. Because `match.call()` returns unevaluated language objects, these variables are assigned the raw AST node (e.g., the call `` `:`(0, 2) ``) rather than the evaluated value (e.g., `c(0L, 1L, 2L)`). This causes downstream failures such as `"non-numeric argument to binary operator"` when `fev` is used in arithmetic inside `rp_simplex()`.

The bug is latent when using defaults (the `else` branch evaluates normally) and when passing bare literals like `TRUE`/`FALSE` (which are self-evaluating constants). It surfaces when:

- Passing expressions: `random_portfolios(p, rp_method = "simplex", fev = 0:2)`
- Passing variables: `my_fev <- 0:3; random_portfolios(p, rp_method = "simplex", fev = my_fev)`

The fix wraps both extractions in `eval.parent()`, matching the pattern already used for the `Multicore` parameter in the same function:

```r
# Before (upstream)
if(hasArg(fev)) fev = match.call(expand.dots=TRUE)$fev else fev = 0:5

# After (this fork)
if(hasArg(fev)) fev = eval.parent(match.call(expand.dots=TRUE)$fev) else fev = 0:5
```

### Bug Fix: Optional Solver Arguments Passed as `NULL` Crashed `optimize.portfolio_v1()`

`hasArg()` is `TRUE` for an argument passed explicitly as `NULL`, so the legacy v1 DEoptim block took its "supplied" branch on a `NULL` and then failed in three different ways, each surfacing far from its cause (verified on R 4.6.0):

| Argument | Guard shape | Result with `NULL` |
|---|---|---|
| `itermax` | `ifelse(is.na(x), TRUE, x)` | `logical(0)` → `NP` becomes `numeric(0)` → `"argument is of length zero"`, before DEoptim is ever invoked |
| `strategy`, `reltol`, `steptol`, `c`, `storepopfrom`, `parallel` | `!hasArg(x) \|\| is.na(...)` | `FALSE \|\| logical(0)` is `NA` → `"missing value where TRUE/FALSE needed"` |
| `packages` | same | `is.na()` on a character vector returns a vector; since R 4.3 `\|\|` errors with `"'length = 2' in coercion to 'logical(1)'"` — making that documented argument impossible to supply |

Omitting an argument always worked; passing it as `NULL` was fatal. The two now mean the same thing.

The fix adds an internal `.pa_arg_missing()` — `NULL`, zero-length and a length-one `NA` count as "not supplied", everything else (including multi-element vectors) as supplied — and applies it to all seven guards, each argument now resolved **once** instead of calling `eval.parent(match.call(...))` twice:

```r
# Before (upstream) — NULL takes the "supplied" branch and collapses
if(hasArg(itermax)) { itermax <- eval.parent(match.call(expand.dots=TRUE)$itermax)
                      itermax <- ifelse(is.na(itermax), yes = TRUE, no = itermax) }
else { itermax = N*50 }

# After (this fork) — NULL/NA mean "not supplied", as omitting it does
.itermax_arg <- if (hasArg(itermax)) eval.parent(match.call(expand.dots = TRUE)$itermax) else NULL
itermax_supplied <- !.pa_arg_missing(.itermax_arg)
itermax <- if (itermax_supplied) .itermax_arg else N * 50
```

This also fixes a defect independent of `NULL`: the old `ifelse()` mapped an `NA` `itermax` to `TRUE` (i.e. `1`), which would have made `NP = search_size`.

The modern v2 path (`solve_deoptim()`) was already `NULL`-safe, since `is.null(x) || is.na(x)` short-circuits — but it carried the same `packages` vector defect, now fixed by testing `length(dots$packages)` rather than `is.na()`.

### Bug Fix: `match.call()` Without `eval.parent()` in `constrained_objective.R` and `ac_ranking.R`

The same `match.call()` anti-pattern appeared in two more files:

- **`constrained_objective.R`**: `verbose` parameter in both `constrained_objective_v2` (1 instance) and `constrained_objective_v1` (1 instance). When passed as a variable, `isTRUE(verbose)` silently returned `FALSE` instead of the intended value. Additionally, `optimize_method` was extracted via `match.call()` but never used in either function body — this dead code was removed.
- **`ac_ranking.R`**: `max.value` parameter in `ac.ranking` (1 instance). When passed as a variable, `scale_range()` would fail with a non-numeric error.

All 3 live instances fixed with `eval.parent()`, 2 dead-code instances removed.

### Bug Fix: `match.call()` Without `eval.parent()` in `custom.covRob.R`

The same `match.call()` anti-pattern from `random_portfolios()` (see above) appeared in all four robust covariance wrapper functions: `custom.covRob.MM` (2 instances), `custom.covRob.Rocke` (6), `custom.covRob.Mcd` (12), and `custom.covRob.TSGS` (7) — 27 instances total.

Each function extracts optional parameters from `...` using `match.call(expand.dots = TRUE)$param` without wrapping in `eval.parent()`. This causes direct calls with variable arguments to fail:

```r
# Before (upstream) — returns unevaluated AST node, fails with "not subsettable"
ctrl <- MycovRobMcd(alpha = 0.75)
custom.covRob.Mcd(R, control = ctrl)  # ERROR

# After (this fork) — eval.parent() evaluates the expression correctly
ctrl <- MycovRobMcd(alpha = 0.75)
custom.covRob.Mcd(R, control = ctrl)  # Works
```

The bug was latent in production because `optimize.portfolio()` calls these functions via `do.call()`, which pre-evaluates all arguments before `match.call()` sees them. The fix applies `eval.parent()` to all 27 instances, matching the `random_portfolios()` fix.

### Bug Fix: Duplicate `barplotWeights` Definition in `chart.Weights.R`

`chart.Weights.R` contained two identical definitions of `barplotWeights()` — the second immediately shadowed the first. In R, when a file is sourced, the last definition wins, so the first definition (lines 37-65) was dead code that could never be called. Coverage tools reported these lines as uncovered. The fix deletes the first duplicate definition.

### Bug Fix: Unused Fallback Variables in Group Constraint Setup (`optFUN.R`)

Eight ROI optimization functions (`gmv_opt`, `maxret_opt`, `gmv_opt_toc`, `gmv_opt_ptc`, `etl_opt_toc`, `etl_opt_ptc`, `gmv_opt_leverage`, `gmv_opt_leverage_toc`) contained identical dead-fallback code for group constraints:

```r
# Before (upstream) — local cLO/cUP created but constraints$cLO used
if(is.null(constraints$cLO)) cLO <- rep(-Inf, n.groups)
if(is.null(constraints$cUP)) cUP <- rep(Inf, n.groups)
...
rhs.vec <- c(rhs.vec, constraints$cLO, -constraints$cUP)  # ignores local cLO/cUP

# After (this fork) — local variables always assigned, then used
cLO <- if(is.null(constraints$cLO)) rep(-Inf, n.groups) else constraints$cLO
cUP <- if(is.null(constraints$cUP)) rep(Inf, n.groups) else constraints$cUP
...
rhs.vec <- c(rhs.vec, cLO, -cUP)
```

If `constraints$cLO` was NULL, `c(rhs.vec, NULL, ...)` would silently drop the NULL, making `rhs.vec` too short for the constraint matrix. If `constraints$cUP` was NULL, `-NULL` would throw `"invalid argument to unary operator"`. The bug was dormant because `group_constraint()` requires non-NULL `group_min`/`group_max`, but it would surface if anyone constructed a constraints list manually.

### Bug Fix: Missing Neighbors Handling in `chart.Scatter.pso`

`chart.Scatter.pso` (the PSO risk-reward scatter plot, aliased as `chart.RiskReward.optimize.portfolio.pso`) accepts a `neighbors` parameter in its function signature but never uses it — the parameter is silently ignored. Both `chart.Scatter.RP` and `chart.Scatter.DE` have active neighbors handling (vector of portfolio indices, single integer for k-nearest, and matrix/data.frame of pre-computed values). The PSO version was missing this block entirely.

The fix inserts the same neighbors handling code (matching the corrected RP version — see next bug fix).

### Bug Fix: Wrong Variable in `chart.Scatter.DE` Matrix Neighbors Fallback

In `chart.Scatter.DE`, the matrix/data.frame neighbors code path has a variable name error in the `pmatch` fallback for `risk.col`:

```r
// Before (upstream) — assigns to wrong variable
rsc = pmatch(risk.col, columnnames)
if(is.na(rsc)) {
  risk.column = pmatch(paste(risk.col,risk.col,sep='.'), columnnames)  // should be rsc
}
for(i in 1:nrow(neighbors)) points(neighbors[i, rsc], ...)  // rsc is still NA
```

If the initial `pmatch` returns `NA`, the fallback writes to `risk.column` instead of `rsc`, leaving `rsc` as `NA`. The `for` loop then indexes `neighbors[i, NA]`, producing invisible/missing points. The `chart.Scatter.RP` version already has the correct assignment (`rsc = pmatch(...)`). The fix is a one-variable-name change.

### Bug Fix: Missing Accumulation in Multi-Factor Residual Cokurtosis

The C function `residualcokurtosisMF()` (in `src/residualcokurtosisMF.c`) computes the residual cokurtosis tensor for statistical factor models with k > 1 factors. The residual tensor element `kijkl` for the case where two pairs of indices match (`i==k && j==l`, with `i != j`) requires three terms:

```
kijkl = betacov[i,i] * stockM2[j] + betacov[j,j] * stockM2[i] + stockM2[i] * stockM2[j]
```

The upstream code splits this across two statements using an accumulator pattern, but the second assignment overwrites the first term instead of adding to it:

```c
// Before (upstream) — drops first term
kijkl = betacov[pos]*stockM2[j];       // term 1
pos = j*N+j;
kijkl = betacov[pos]*stockM2[i]+stockM2[i]*stockM2[j];  // overwrites term 1

// After (this fork) — accumulates correctly
kijkl = betacov[pos]*stockM2[j];       // term 1
pos = j*N+j;
kijkl = kijkl + betacov[pos]*stockM2[i]+stockM2[i]*stockM2[j];  // adds to term 1
```

The other two symmetric cases (`i==j && k==l` and `i==l && j==k`) correctly use `kijkl = kijkl + ...`. The single-factor specialization (`residualcokurtosisSF.c`) is also correct — it computes all three terms in a single expression.

The bug affects `N * (N-1)` elements per cokurtosis matrix (the off-diagonal positions of the `i==k && j==l` symmetry class). Each affected element is underestimated by exactly `betacov[i,i] * stockM2[j]`. This impacts any code path that calls `extractCokurtosis()` on a statistical factor model with k >= 2.

### Bug Fix: `dimnames` Mismatch in `meanrisk.efficient.frontier`

`meanrisk.efficient.frontier()` (in `extract.efficient.frontier.R`) computes a multi-risk efficient frontier by optimizing portfolios for each risk measure in `compare_port`, then assembles the results into a matrix. The column-naming step on line 540 has a bug caused by R's `paste()` recycling behavior with zero-length vectors:

```r
# Line 477: remove risk_type from compare_port to get the "comparison" set
risk_compare <- compare_port[-which(compare_port == risk_type)]

# Line 540: name the extra columns — one per comparison risk measure
colnames(out) <- c(names(stats), paste(risk_compare, 'portfolio', risk_type))
```

When `risk_compare` is `character(0)` (i.e. nothing left to compare), `paste(character(0), 'portfolio', risk_type)` does **not** return `character(0)`. Instead, R recycles the zero-length first argument to `""` against the non-zero-length `risk_type` argument, producing phantom column names like `" portfolio StdDev"`. The `colnames<-` assignment then fails with `"length of 'dimnames' [2] not equal to array extent"` because the matrix has no extra data columns to match these phantom names.

**Triggers:**
- `compare_port = c("StdDev")` with `risk_type = "StdDev"` — all entries removed, `risk_compare = character(0)`
- `risk_type` passed as a vector (e.g. `c("StdDev", "ES")`) — element-wise `==` removes all entries, and the multi-element `risk_type` amplifies the phantom names

The function also lacked input validation: `risk_type` is documented as a single string but was not checked, and downstream code (`which(... == risk_type)`, `extract_risk(...)[[risk_type]]`) assumes scalar semantics.

**Impact:** `chart.EfficientFrontierCompare()` calls `meanrisk.efficient.frontier()` via `create.EfficientFrontier(type = "mean-risk")`. The bug didn't surface with the most common parameter combination (`risk_type = "StdDev"`, `compare_port = c("StdDev", "ES")`), but would crash on degenerate inputs.

The fix adds input validation (`risk_type` must be a single character string, `compare_port` entries must be valid risk types) and guards the `colnames` assignment so that an empty `risk_compare` produces `character(0)` instead of phantom names.

### Bug Fix: Uninitialized `dotargs` When `momentFUN` Fails in `optimize.portfolio`

In the v2 `optimize.portfolio()` code path, the portfolio moment function is called inside `try()` and, on success, assigns the result to `dotargs`. On failure, the upstream code calls `message()` with the error text and continues — but `dotargs` is never assigned. Two downstream uses (`calibrate_penalty(..., env = dotargs)` and `solver_fn(..., moments = dotargs, ...)`) then fail with `object 'dotargs' not found`, masking the original moment function error.

The v1 code path does not have this bug because it initializes `dotargs <- list(...)` before the `try()` call, so `dotargs` always exists even if `momentFUN` fails.

The fix replaces `message()` with `stop()`, since optimization cannot proceed without moments. The error message includes the original function name and the underlying failure text:

```r
# Before (upstream) — message + continue, then crash on undefined dotargs
if(inherits(mout, "try-error")) {
  message(paste("portfolio moment function failed with message", mout))
} else {
  dotargs <- mout
}

# After (this fork) — stop immediately with actionable error
if(inherits(mout, "try-error")) {
  stop("Portfolio moment function ('", moment_name,
       "') failed. Cannot proceed without moments.\n",
       "Original error: ", mout, call. = FALSE)
} else {
  dotargs <- mout
}
```

### Upstream Sync: `CVXR::solve()` → `CVXR::psolve()` Migration

CVXR deprecated `solve()` in favor of `psolve()`. This fork migrated all call sites:

- **`solver_cvxr.R`** (line 244): The centralized `.cvxr_solve()` wrapper uses `CVXR::psolve()`
- **`solver_cvxr.R`** (line 269): Result extraction uses `CVXR::value(wts)` (replacing the old `$getValue(wts)`)
- **`extractrisk.R`**: All three `CVXR::solve()` calls (ES, CSM, EQS) replaced with `CVXR::psolve()`

Upstream commits `951f3c6` and `68e7431` (March 2026) make the same `psolve` migration. The following upstream changes were **not** taken:

- **CSM formula change** (`sqrt(T)` removed from `extractrisk.R`): Upstream's own CVXR solver at line 2915 of `optimize.portfolio.R` still uses `1/(alpha * sqrt(T))`. This fork keeps `sqrt(T)` in both `solver_cvxr.R` and `extractrisk.R` for internal consistency.
- **Merge conflict markers in `optimize.portfolio.R`**: Upstream lines 2929 and 2946 contain literal `<` and `=` prefixes from an unresolved merge, which would cause R parse errors. Not applicable to this fork (CVXR solver lives in `solver_cvxr.R`).

The upstream vignette URL fix (missing `/` in a CRAN link) and `extractrisk.R` style cleanup (`=` → `<-`) were taken.

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

The package includes 103 test files with 3,155+ passing assertions:

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
- **Architecture docs**: See `architecture.md` in this repository

## License

GPL-3
