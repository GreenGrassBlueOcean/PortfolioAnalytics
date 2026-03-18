##### test_no_triple_colon.R #####
# Guard: test files must not use PortfolioAnalytics::: or PortfolioAnalytics::
# and must not contain redundant library/require calls for PortfolioAnalytics
# or testthat. Both are already loaded by the test harness.

test_that("no test file uses PortfolioAnalytics::: or PortfolioAnalytics::", {
  test_dir <- system.file("tests", "testthat", package = "PortfolioAnalytics")
  if (!nzchar(test_dir)) test_dir <- "."
  test_files <- list.files(test_dir, pattern = "^test.*\\.R$", full.names = TRUE)
  test_files <- test_files[!grepl("test_no_triple_colon\\.R$", test_files)]

  violations <- character(0)
  for (f in test_files) {
    lines <- readLines(f, warn = FALSE)
    code_lines <- lines[!grepl("^\\s*#", lines)]
    hits3 <- grep("PortfolioAnalytics:::", code_lines)
    if (length(hits3) > 0) {
      violations <- c(violations, paste0(basename(f), ": ", length(hits3),
                                         " ::: call(s)"))
    }
    hits2 <- grep("PortfolioAnalytics::[^:]", code_lines)
    if (length(hits2) > 0) {
      violations <- c(violations, paste0(basename(f), ": ", length(hits2),
                                         " :: call(s)"))
    }
  }
  expect_equal(violations, character(0),
               info = paste(violations, collapse = "\n"))
})

test_that("no test file has redundant library/require calls", {
  test_dir <- system.file("tests", "testthat", package = "PortfolioAnalytics")
  if (!nzchar(test_dir)) test_dir <- "."
  test_files <- list.files(test_dir, pattern = "^test.*\\.R$", full.names = TRUE)
  test_files <- test_files[!grepl("test_no_triple_colon\\.R$", test_files)]

  redundant_pkgs <- c("PortfolioAnalytics", "testthat")
  pattern <- paste0("^\\s*(library|require)\\((",
                     paste(redundant_pkgs, collapse = "|"), ")\\)")

  violations <- character(0)
  for (f in test_files) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep(pattern, lines)
    if (length(hits) > 0) {
      violations <- c(violations, paste0(basename(f), " line(s) ",
                                         paste(hits, collapse = ","), ": ",
                                         trimws(lines[hits[1]])))
    }
  }
  expect_equal(violations, character(0),
               info = paste(violations, collapse = "\n"))
})
