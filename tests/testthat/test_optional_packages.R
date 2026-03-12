# Tests for Proposal #10: Optional packages in Suggests
# Verifies DESCRIPTION structure and requireNamespace guards.

library(testthat)

# Helper: locate the package source root (works both interactively and during R CMD check)
pkg_source_root <- function() {
  root <- testthat::test_path("..", "..")
  if (file.exists(file.path(root, "DESCRIPTION"))) return(root)
  NULL
}

# Helper: read the DESCRIPTION from source tree; skip if unavailable
read_pkg_desc <- function() {
  root <- pkg_source_root()
  if (is.null(root)) skip("DESCRIPTION not found (likely running from installed package)")
  read.dcf(file.path(root, "DESCRIPTION"))
}

# --- Test 1: Only 'methods' remains in Imports ---
test_that("only 'methods' is in Imports", {
  desc <- read_pkg_desc()
  imports_raw <- desc[1, "Imports"]
  imports_pkgs <- unname(trimws(gsub("\\(.*\\)", "", 
                                      trimws(unlist(strsplit(imports_raw, ",\\s*"))))))
  expect_equal(imports_pkgs, "methods")
})

# --- Helper used in subsequent tests ---
get_suggests <- function() {
  desc <- read_pkg_desc()
  raw <- desc[1, "Suggests"]
  unname(trimws(gsub("\\(.*\\)", "", trimws(unlist(strsplit(raw, ",\\s*"))))))
}

# --- Test 2: Parallel packages are in Suggests ---
test_that("doParallel, snow, doSNOW are in Suggests", {
  pkgs <- get_suggests()
  expect_true("doParallel" %in% pkgs)
  expect_true("snow" %in% pkgs)
  expect_true("doSNOW" %in% pkgs)
})

# --- Test 3: Solver packages remain in Suggests (no regression) ---
test_that("solver packages are in Suggests", {
  pkgs <- get_suggests()
  expect_true("DEoptim" %in% pkgs)
  expect_true("GenSA" %in% pkgs)
  expect_true("pso" %in% pkgs)
  expect_true("ROI" %in% pkgs)
})

# --- Test 4: mco is not a dependency ---
test_that("mco is not listed as a dependency", {
  desc <- read_pkg_desc()
  strip <- function(field) {
    val <- desc[1, field]
    if (is.na(val)) return(character(0))
    unname(trimws(gsub("\\(.*\\)", "", trimws(unlist(strsplit(val, ",\\s*"))))))
  }
  all_pkgs <- c(strip("Imports"), strip("Suggests"), strip("Depends"))
  expect_false("mco" %in% all_pkgs)
})

# --- Test 5: C source files exist ---
test_that("C source files exist in src/", {
  root <- pkg_source_root()
  skip_if(is.null(root), "src/ directory not found")
  src_dir <- file.path(root, "src")
  skip_if_not(dir.exists(src_dir), "src/ directory not found")
  c_files <- list.files(src_dir, pattern = "\\.c$")
  expect_true(length(c_files) >= 3)
})

# --- Test 6: requireNamespace guard in solver_deoptim.R ---
test_that("solver_deoptim.R has requireNamespace guard for snow/doSNOW", {
  root <- pkg_source_root()
  skip_if(is.null(root), "solver_deoptim.R not found")
  src_file <- file.path(root, "R", "solver_deoptim.R")
  skip_if_not(file.exists(src_file), "solver_deoptim.R not found")
  src <- readLines(src_file)
  expect_true(any(grepl('requireNamespace\\("snow"', src)))
  expect_true(any(grepl('requireNamespace\\("doSNOW"', src)))
})

# --- Test 7: requireNamespace guard in random_portfolios.R ---
test_that("random_portfolios.R has requireNamespace guard for doParallel", {
  root <- pkg_source_root()
  skip_if(is.null(root), "random_portfolios.R not found")
  src_file <- file.path(root, "R", "random_portfolios.R")
  skip_if_not(file.exists(src_file), "random_portfolios.R not found")
  src <- readLines(src_file)
  expect_true(any(grepl('requireNamespace\\("doParallel"', src)))
})

# --- Test 8: requireNamespace guard in optimize.portfolio.R ---
test_that("optimize.portfolio.R has requireNamespace guard for snow/doSNOW", {
  root <- pkg_source_root()
  skip_if(is.null(root), "optimize.portfolio.R not found")
  src_file <- file.path(root, "R", "optimize.portfolio.R")
  skip_if_not(file.exists(src_file), "optimize.portfolio.R not found")
  src <- readLines(src_file)
  expect_true(any(grepl('requireNamespace\\("snow"', src)))
  expect_true(any(grepl('requireNamespace\\("doSNOW"', src)))
})
