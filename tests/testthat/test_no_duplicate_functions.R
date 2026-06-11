# Guard: Check for duplicate function definitions across test files

test_that("no duplicate functions across test files", {
  test_dir <- system.file("tests", "testthat", package = "PortfolioAnalytics")
  if (!nzchar(test_dir)) test_dir <- "."
  test_files <- list.files(test_dir, pattern = "^test.*\\.R$", full.names = TRUE)

  # Collect all function definitions
  func_defs <- list()

  for (f in test_files) {
    lines <- readLines(f, warn = FALSE)
    # Matches simple function definitions like "name <- function(...)"
    matches <- grep("^\\s*([a-zA-Z0-9_\\.]+)\\s*<-\\s*function\\(", lines, value = TRUE)
    
    for (m in matches) {
      name <- sub("^\\s*([a-zA-Z0-9_\\.]+)\\s*<-.*", "\\1", m)
      # Skip standard testthat constructs if they accidentally match
      if (name %in% c("expect_error", "test_that", "error", "warning")) next
      
      if (!is.null(func_defs[[name]])) {
        func_defs[[name]] <- c(func_defs[[name]], basename(f))
      } else {
        func_defs[[name]] <- basename(f)
      }
    }
  }

  duplicates <- list()
  for (name in names(func_defs)) {
    if (length(unique(func_defs[[name]])) > 1) {
      duplicates[[name]] <- unique(func_defs[[name]])
    }
  }

  error_msg <- character(0)
  if (length(duplicates) > 0) {
    for (name in names(duplicates)) {
      error_msg <- c(error_msg, paste0("Function '", name, "' is defined in multiple files: ", 
                                       paste(duplicates[[name]], collapse = ", ")))
    }
  }

  expect_equal(error_msg, character(0),
               info = paste(error_msg, collapse = "\n"))
})
