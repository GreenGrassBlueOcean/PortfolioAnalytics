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
