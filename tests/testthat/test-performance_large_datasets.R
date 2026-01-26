# ==============================================================================
# ISSUE #420: check_model() performance degradation with large datasets
# ==============================================================================
#
# This test documents the performance issue where check_model() becomes
# unusably slow (5+ minutes) with datasets >10K observations.
#
# The test verifies that plotting completes in reasonable time (<30 seconds)
# for large datasets.
#
# See: https://github.com/easystats/see/issues/420
# ==============================================================================

# CHANGE 1: Renamed test and removed unnecessary skip_if_not_installed("lme4")
test_that("check_model() runs without error on small datasets", {
  skip_if_not_installed("performance")

  # Small dataset should not be sampled
  small_data <- data.frame(
    x = rnorm(100),
    y = rnorm(100)
  )
  model_small <- lm(y ~ x, data = small_data)

  # Should complete quickly regardless
  expect_no_error({
    result <- performance::check_model(model_small)
    plot(result)
  })
})


# CHANGE 2: Renamed variable from large_data to large_data_lm
test_that("large dataset plot components use sampling", {
  skip_if_not_installed("performance")
  skip_on_cran()

  # Create dataset with 15,000 observations (above threshold)
  set.seed(456)
  large_data_lm <- data.frame(
    x = rnorm(15000),
    y = rnorm(15000) + 0.5 * rnorm(15000)
  )

  model <- lm(y ~ x, data = large_data_lm)

  # Check that plotting completes quickly
  timing <- system.time({
    result <- performance::check_model(model)
    plot_result <- plot(result)
  })

  # Should be much faster with sampling
  expect_true(
    timing["elapsed"] < 15,
    info = sprintf(
      "Plotting took %.1f seconds (should be <15 with sampling)",
      timing["elapsed"]
    )
  )
})


test_that("medium datasets complete in reasonable time", {
  skip_if_not_installed("performance")
  skip_on_cran()

  # Medium dataset (8,000 observations - below threshold)
  set.seed(789)
  medium_data <- data.frame(
    x = rnorm(8000),
    y = rnorm(8000) + rnorm(8000)
  )

  model <- lm(y ~ x, data = medium_data)

  # Should complete in reasonable time even without sampling
  timing <- system.time({
    result <- performance::check_model(model)
    plot_result <- plot(result)
  })

  # Medium datasets should still be reasonably fast
  expect_true(
    timing["elapsed"] < 20,
    info = sprintf(
      "Medium dataset plotting took %.1f seconds",
      timing["elapsed"]
    )
  )
})
