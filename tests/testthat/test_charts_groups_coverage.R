context("charts.groups.R coverage: chart.GroupWeights + barplotGroupWeights")


data(edhec, package = "PerformanceAnalytics")
R5 <- edhec[1:48, 1:5]
colnames(R5) <- c("A", "B", "C", "D", "E")

# Build portfolio with group constraints
portf_g <- portfolio.spec(assets = colnames(R5))
portf_g <- add.constraint(portf_g, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
portf_g <- add.constraint(portf_g, type = "box", min = 0.05, max = 0.55)
portf_g <- add.constraint(portf_g, type = "group",
                          groups = list(c(1, 2), c(3, 4, 5)),
                          group_min = c(0.2, 0.3),
                          group_max = c(0.5, 0.7),
                          group_labels = c("GroupAB", "GroupCDE"))
portf_g <- add.objective(portf_g, type = "risk", name = "StdDev")
portf_g <- add.objective(portf_g, type = "return", name = "mean")

set.seed(8291)
opt_g <- optimize.portfolio(R5, portf_g, optimize_method = "random",
                            search_size = 500, trace = TRUE)

# Build portfolio with category labels
portf_cat <- portfolio.spec(assets = colnames(R5))
portf_cat <- add.constraint(portf_cat, type = "weight_sum",
                            min_sum = 0.99, max_sum = 1.01)
portf_cat <- add.constraint(portf_cat, type = "box", min = 0.05, max = 0.55)
portf_cat <- add.objective(portf_cat, type = "risk", name = "StdDev")
portf_cat$category_labels <- list(Equity = c(1, 2), Fixed = c(3, 4, 5))

set.seed(6384)
opt_cat <- optimize.portfolio(R5, portf_cat, optimize_method = "random",
                              search_size = 500, trace = TRUE)

# ============================================================================
# A. chart.GroupWeights with line plot (groups)
# ============================================================================

test_that("chart.GroupWeights line plot with group constraints", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.GroupWeights(opt_g, grouping = "groups", plot.type = "line"))
})

# ============================================================================
# B. chart.GroupWeights with line plot (category)
# ============================================================================

test_that("chart.GroupWeights line plot with category grouping", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.GroupWeights(opt_cat, grouping = "category", plot.type = "line"))
})

# ============================================================================
# C. barplotGroupWeights (bar plot mode)
# ============================================================================

test_that("chart.GroupWeights bar plot with group constraints", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.GroupWeights(opt_g, grouping = "groups", plot.type = "bar"))
})

test_that("chart.GroupWeights barplot with category grouping", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(chart.GroupWeights(opt_cat, grouping = "category", plot.type = "barplot"))
})

# ============================================================================
# D. Styling parameters
# ============================================================================

test_that("chart.GroupWeights handles custom styling", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.GroupWeights(opt_g, grouping = "groups", plot.type = "line",
                       main = "Custom Title", las = 1,
                       xlab = "Groups", cex.lab = 1.0,
                       element.color = "black", cex.axis = 1.0)
  )
})

test_that("chart.GroupWeights handles empty main title", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.GroupWeights(opt_g, grouping = "groups", plot.type = "line",
                       main = "")
  )
})

# ============================================================================
# E. Error handling
# ============================================================================

test_that("chart.GroupWeights errors on non-portfolio object", {
  expect_error(chart.GroupWeights(list()), "optimize.portfolio")
})

# ============================================================================
# F. Long group names (triggers margin truncation)
# ============================================================================

test_that("chart.GroupWeights handles long group labels", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  # Modify group_labels to be very long
  opt_long <- opt_g
  opt_long$portfolio$constraints[[3]]$group_labels <- 
    c("VeryLongGroupNameThatExceeds19Chars", "AnotherExcessivelyLongGroupName")
  expect_no_error(
    chart.GroupWeights(opt_long, grouping = "groups", plot.type = "line",
                       las = 3)
  )
})
