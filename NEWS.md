# PortfolioAnalytics 2.1.1.9009

## Enhancements

* `solve_deoptim()` now ships the moment list to its workers once instead of on every generation. DEoptim's cluster path evaluates a generation with `parApply(cl, pop, 1, fn, ...)`, which re-serializes `FUN` and every `...` argument to every worker each time it is called, and `env = moments` was passed that way. For CRRA the moment list carries the `m4` co-moment tensor, `N x N^3`: at 60 assets that is 103.7 MB by itself, so an 18-worker cluster moved ~1.87 GB per generation and ~370 GB over a 200-generation run, for a value that never changes after `momentFUN` runs once. Measured on 4 local workers with a 98.9 MB payload the cost was 0.60-0.75 s **per generation** and linear in `itermax`, confirming transport rather than a setup cost. The moments now go across in a single `clusterCall` and the objective reads them from a namespace-level cache. Results are unchanged; `moments_cache = FALSE` restores the previous transport for comparison.

## Testing

* `test-moments-transport.R` runs three arms on one seed — parallel with the new transport, parallel with the old one (`moments_cache = FALSE`), and single-core — and requires all three to agree bit-identically (`tolerance = 0`). The first pair is the before/after check on the transport itself; the second pair re-asserts the parallel/sequential agreement won in 2.1.1.9007, which a change to how the objective reaches the workers is exactly the sort of thing to break again. There is a guard that fails the test if no cluster was actually built — otherwise both arms take the same branch and the comparison is vacuous. It also pins the environment and serialized size of the cached objective: this optimisation is undone silently if that wrapper is ever moved inside `solve_deoptim()`, because its closure would then capture the frame holding `moments` and every generation would ship the tensor again with no change to any result.

# PortfolioAnalytics 2.1.1.9008

## Bug Fixes

* Applied the 2.1.1.9007 RNG guard and the 2.1.1.9006 worker-attachment fix to `optimize.portfolio_v1()`, which carried both defects unchanged. Nothing in the current dispatch path reaches `_v1`, but it is exported, and the `v1`/`v2` split had by then hidden three separate bugs in this package (`MaxCores` forwarded but unread, `constrained_objective()`'s swallowed `try-error`, and this).

## Testing

* `test-parallel-worker-equivalence.R` gains a `_v1` case. The RNG tests run in a fresh subprocess deliberately: once `snow`/`doSNOW` are loaded in a session the shift cannot recur, so an in-session test would silently stop protecting anything. Each test was validated by disabling its fix and confirming it fails.

# PortfolioAnalytics 2.1.1.9007

## Bug Fixes

* Fixed parallel and sequential runs returning different optima from the same seed. Loading `doSNOW` consumes one draw from the global RNG stream, and `requireNamespace("doSNOW")` runs only on the parallel path, so a parallel run started DEoptim one draw further along than a sequential one: different initial population, different mutation sequence, different result from identical inputs. It was reproducible *within* a mode (two parallel runs bit-identical), which disguised an RNG-position problem as a scoring problem. `solve_deoptim()` now snapshots `.Random.seed` before cluster setup and restores it afterwards. Measured on a 9-fold walk-forward before the fix: Sharpe 0.4535 parallel vs 0.5778 sequential, one weight differing by 0.287. (`snow` and `doSNOW` shift the stream; `foreach`, `iterators`, `doParallel` and `DEoptim` do not.)

# PortfolioAnalytics 2.1.1.9006

## Bug Fixes

* Fixed worker package attachment. `clusterEvalQ(rcl, lapply(names(sessionInfo()$otherPkgs), require, ...))` evaluates on the *worker*, where nothing is attached, so `otherPkgs` was `NULL` there and it loaded nothing at all. `constrained_objective()` special-cases a few measures by symbol (`StdDev`, `VaR`, `ES`, `mean`, `median`, `turnover`) but resolves everything else — `CRRA` included — with `match.fun(objective$name)`, a string lookup against the caller's frame, which on a bare worker cannot reach this namespace. The package list is now computed in the master and shipped with `clusterCall()`.
* `constrained_objective()` now raises when an objective cannot be evaluated, naming it. Previously the `try-error` was reported with `message()` and then used in arithmetic anyway, so the only visible symptom was `non-numeric argument to binary operator` several lines later. This changes no result: every branch multiplies the measure, so a `try-error` already killed the run one line on. Fixed in both `_v1` and `_v2`.

# PortfolioAnalytics 2.1.1.9005

## Enhancements

* `normalize` is now settable through `...` on the DEoptim path (default `FALSE`, so existing results are unchanged). It controls whether a candidate that misses the leverage band is rescaled onto it or penalised, which materially changes how the search behaves on a narrow band.

# PortfolioAnalytics 2.1.1.9004

## Bug Fixes

* Fixed `MaxCores` being silently ignored by `optimize.portfolio()`. It was declared as a formal but never read, and because it is a formal it was also absent from `...`, so it never reached the solver: `solve_deoptim()` built `parallel::makeCluster(min(nC, 15))` regardless of what the caller asked for. A caller requesting `MaxCores = 12` still got 15 workers per concurrent optimisation. `optimize.portfolio_v1()` honours `MaxCores` correctly, which is why this went unnoticed — the legacy path worked and the `_v2` path silently did not.
* Fixed `MaxSubCores` being inert as a consequence of the above. `optimize.portfolio.rebalancing()` forwards it as `MaxCores` to the inner `optimize.portfolio()` calls precisely to bound the nested cluster each rebalance-period worker builds; since that value was discarded, nesting was unbounded and the machine could be oversubscribed by *outer × 15* workers.

## Enhancements

* `solve_deoptim()` reports the realised cluster size under `message = TRUE`: `DEoptim parallel cluster: N worker(s) (detectCores=..., MaxCores=...)`. Previously nothing was logged, so a cluster of the wrong size left no trace.
* `solve_deoptim()` warns when `parallel = TRUE` but `foreach` is not attached. That combination runs the whole optimisation sequentially and previously said nothing at all, so the only symptom was a run taking many times longer for no visible reason.

# PortfolioAnalytics 2.1.1.9001

## Bug Fixes

* Fixed a severe mathematical formulation bug in `gmv_opt_ptc` (Proportional Transaction Costs). Linear constraints (box, group, and factor constraints) were incorrectly being applied to the transaction cost auxiliary variables instead of just the portfolio weights.
* Fixed the `maxSR = TRUE` (Maximum Sharpe Ratio) logic in `solve_roi` where custom expected returns (e.g., from an alpha model) were being silently ignored. Previously, `roi_moments$mean` was zeroed out immediately prior to the final tangency portfolio solve, which forced `gmv_opt` to fallback to historical column means (`colMeans(R)`). Retaining `roi_moments$mean` ensures the final optimization respects custom expected return models and remains consistent with the tangency portfolio search.
* Fixed short position limit logic in the Random Portfolio solver (`rp_position_limit`). Changed the evaluation threshold from `weight < tolerance` to `weight < -tolerance` to prevent zero or microscopically small positive weights from being misclassified as short positions.
* Fixed weight clamping in the CVXR solver (`solve_cvxr`). Post-optimization weights are now only clamped to box bounds if the boundary violation is strictly within numerical tolerance (`1e-5`). Unconditional clamping previously masked real infeasibilities and silently returned mathematically invalid portfolios.
* Fixed `maxret_milp_opt` to correctly use `constraints$max_sum` and `constraints$min_sum` for the linear weight sum constraints instead of hardcoding them to 1.
* Fixed convex projection logic in `constraint_fn_map` to properly disable itself when non-convex `group_pos` (group cardinality) constraints are present, preventing the projection from breaking position limits.
* Fixed an R CMD check warning regarding the `filter_constraint` global variable in `add.constraint`.

## Enhancements

* Added comprehensive support for `group_pos` constraints (limiting the maximum number of non-zero positions per group) to the global solvers (DEoptim, random) by adding penalty evaluations in `constrained_objective`.
