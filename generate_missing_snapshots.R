# Script to generate missing vdiffr snapshots for sort parameter tests
# Run this in an R environment with proper dependencies installed

# Install required packages if not already installed
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
if (!requireNamespace("vdiffr", quietly = TRUE)) {
  install.packages("vdiffr")
}
if (!requireNamespace("parameters", quietly = TRUE)) {
  install.packages("parameters")
}
if (!requireNamespace("see", quietly = TRUE)) {
  install.packages("see")
}

# Load libraries
library(testthat)
library(vdiffr)
library(parameters)
library(see)

# Ensure we're in the correct directory (package root)
if (
  !file.exists("DESCRIPTION") ||
    !file.exists("tests/testthat/test-plot.parameters_model.R")
) {
  stop("Please run this script from the package root directory")
}

# Set up the test environment
withr::local_dir(here::here())

# Create test data (matching the test file)
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- parameters::model_parameters(m)

# Generate the missing snapshots
message("Generating vdiffr snapshots for sort parameter tests...")

# Test context setup
testthat::test_that("Generate missing sort parameter snapshots", {
  
  # Generate ascending sort snapshot
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_sort_ascending",
    fig = plot(result, sort = "ascending", show_labels = TRUE)
  )

  # Generate descending sort snapshot
  vdiffr::expect_doppelganger(
    title = "plot.model_parameters_sort_descending",
    fig = plot(result, sort = "descending", show_labels = TRUE)
  )
  
})

message("Snapshots should now be generated in tests/testthat/_snaps/plot.parameters_model/")
message("Expected files:")
message("- plot-model-parameters-sort-ascending.svg")  
message("- plot-model-parameters-sort-descending.svg")