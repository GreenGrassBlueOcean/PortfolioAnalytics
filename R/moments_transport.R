###############################################################################
# One-shot moment transport for the parallel DEoptim path
###############################################################################
#
# WHY THIS EXISTS
#
# DEoptim's cluster path evaluates an entire generation with
#
#     fnPop <- function(`*params`, ...)
#         parallel::parApply(cl = ctrl$cluster, X = `*params`, MARGIN = 1,
#                            FUN = fn, ...)
#
# and `parApply` ships FUN together with EVERY `...` argument to EVERY worker on
# EVERY generation. `solve_deoptim()` used to pass `env = moments` that way. For
# CRRA the moment list carries the m4 co-moment tensor, which is N x N^3: at 60
# assets that is 12,960,000 doubles, 103.7 MB, on its own. An 18-worker cluster
# therefore moved ~1.87 GB per generation and ~370 GB across a 200-generation
# run, for a value that never changes after `momentFUN` runs once.
#
# Measured on 4 local PSOCK workers with a 98.9 MB payload:
#
#     itermax  3   overhead 2.25 s   (0.75 s per generation)
#     itermax  6   overhead 3.86 s   (0.64 s per generation)
#     itermax 12   overhead 7.23 s   (0.60 s per generation)
#
# Linear in the generation count, so it is transport, not a setup cost. (It is
# also not recomputation: `optimize.portfolio()` calls `momentFUN` exactly once
# and `constrained_objective()` uses the `env` it is handed.)
#
# THE FIX, AND THE TRAP IN IT
#
# Ship the moments once into a namespace-level cache on each worker and hand
# DEoptim a wrapper that reads from there instead of receiving them as data.
#
# The wrapper and the setter MUST be defined here, at namespace level. A
# wrapper defined inside `solve_deoptim()` would close over that frame -- which
# holds `moments` -- and R would serialize the whole 103.7 MB along with the
# function anyway, silently undoing the entire optimisation. Nothing would
# error; the run would simply stay slow. `test-moments-transport.R` pins the
# wrapper's environment identity for exactly this reason -- a closure whose
# environment IS the namespace cannot have captured a local `moments`, because
# a namespace is not a call frame.
#
# A closure whose environment is a namespace serializes as a reference to that
# namespace, which is what makes the worker load PortfolioAnalytics on
# unserialize without any help from us. Do not read that as "serializes to a
# pointer": the measured size varies a lot by platform and R version (~30 KB on
# Windows/R 4.5.2, ~867 KB on macOS/R 4.6.1). Either way it is under 1% of the
# 103.7 MB it replaces, and the same cost was already being paid to ship
# `constrained_objective` itself before this change.

# Home for the cached moment list, on whichever process is asking.
#
# Parented to emptyenv() so nothing can be reached through it by accident.
.pa_moments_cache_env <- new.env(parent = emptyenv())

#' Store the moment list in this process's cache
#'
#' Used as the `fun` of a single `parallel::clusterCall()`, so it must live at
#' namespace level -- see the note at the top of this file.
#'
#' @param m The moment list produced by `momentFUN`.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
.pa_set_moments_cache <- function(m) {
  assign("moments", m, envir = .pa_moments_cache_env)
  invisible(NULL)
}

#' Report whether this process can hold a moment cache at all
#'
#' Workers deserialize a namespace closure against THEIR OWN installed
#' PortfolioAnalytics, not the master's. If a worker resolves an older copy --
#' easily done when a stale build sits earlier on its `.libPaths()`, which is a
#' real configuration we have seen -- it will not have `.pa_moments_cache_env`
#' and the assignment errors on every node. `solve_deoptim()` probes with this
#' first and falls back to passing the moments as data, so a version skew costs
#' throughput rather than the whole run.
#'
#' @return `TRUE` on a process whose namespace provides the cache.
#' @keywords internal
#' @noRd
.pa_moments_cache_ready <- function() {
  is.environment(.pa_moments_cache_env)
}

#' Retrieve this process's cached moment list
#'
#' @return The cached moment list, or `NULL` when nothing has been cached.
#' @keywords internal
#' @noRd
.pa_get_moments_cache <- function() {
  if (!exists("moments", envir = .pa_moments_cache_env, inherits = FALSE)) {
    return(NULL)
  }
  get("moments", envir = .pa_moments_cache_env, inherits = FALSE)
}

#' Drop this process's cached moment list
#'
#' Called on exit from `solve_deoptim()` so a 100 MB tensor does not outlive the
#' optimisation that needed it.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
.pa_clear_moments_cache <- function() {
  if (exists("moments", envir = .pa_moments_cache_env, inherits = FALSE)) {
    rm("moments", envir = .pa_moments_cache_env)
  }
  invisible(NULL)
}

#' Objective wrapper that reads moments from the process-local cache
#'
#' Behaves exactly like `constrained_objective()` with `env = <the moments>`;
#' the only difference is where the moments come from.
#'
#' @param w Candidate weight vector, supplied by DEoptim.
#' @param ... Passed through to `constrained_objective()`.
#' @return Whatever `constrained_objective()` returns.
#' @keywords internal
#' @noRd
.pa_cached_objective <- function(w, ...) {
  m <- .pa_get_moments_cache()
  if (is.null(m)) {
    # Reachable only if the cluster was rebuilt underneath us, or if a worker
    # somehow skipped the clusterCall. Say so plainly rather than letting
    # constrained_objective() recompute moments and quietly return a different
    # number from every other worker.
    stop("the moment cache is empty on this process, so the objective cannot ",
         "be scored the way the rest of the population was. This should not ",
         "happen: solve_deoptim() populates it on every worker before the ",
         "search starts.", call. = FALSE)
  }
  constrained_objective(w = w, env = m, ...)
}
