skip_on_cran()

# Snapshot tests for check_model() plots
# These tests verify that diagnostic plots render consistently across versions.
# Related to #236 (snapshot tests) and #420 (large dataset performance).

# Helper function for reproducible vdiffr tests
expect_doppelganger_with_seed <- function(title, fig, seed = 123) {
  set.seed(seed)
  vdiffr::expect_doppelganger(title = title, fig = fig)
}

test_that("plot.see_check_model() renders correctly", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  set.seed(123)
  model <- lm(mpg ~ wt + hp + cyl, data = mtcars)

  # Test linearity check (NCV)
  expect_doppelganger_with_seed(
    title = "check_model_linearity",
    fig = plot(performance::check_model(model, check = "linearity"))
  )

  # Test homogeneity of variance
  expect_doppelganger_with_seed(
    title = "check_model_homogeneity",
    fig = plot(performance::check_model(model, check = "homogeneity"))
  )

  # Test normality (QQ plot)
  expect_doppelganger_with_seed(
    title = "check_model_qq",
    fig = plot(performance::check_model(model, check = "qq"))
  )

  # Test outliers
  expect_doppelganger_with_seed(
    title = "check_model_outliers",
    fig = plot(performance::check_model(model, check = "outliers"))
  )

  # Test full panel plot
  expect_doppelganger_with_seed(
    title = "check_model_panel",
    fig = plot(performance::check_model(model))
  )
})


test_that("plot.see_check_model() works with themes", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  set.seed(123)
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Test with modern theme
  expect_doppelganger_with_seed(
    title = "check_model_theme_modern",
    fig = plot(performance::check_model(model, check = "linearity")) +
      theme_modern()
  )

  # Test with lucid theme
  expect_doppelganger_with_seed(
    title = "check_model_theme_lucid",
    fig = plot(performance::check_model(model, check = "linearity")) +
      theme_lucid()
  )
})


test_that("plot.see_check_model() works with custom colors", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  set.seed(123)
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Test with custom colors
  expect_doppelganger_with_seed(
    title = "check_model_custom_colors",
    fig = plot(
      performance::check_model(model, check = "linearity"),
      colors = c("#1b6ca8", "#cd201f", "#3aaf85")
    )
  )
})


test_that("plot.see_check_normality() renders correctly", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  set.seed(123)
  model <- lm(mpg ~ wt + hp + cyl, data = mtcars)
  norm_result <- performance::check_normality(model)

  expect_doppelganger_with_seed(
    title = "check_normality_default",
    fig = plot(norm_result)
  )

  expect_doppelganger_with_seed(
    title = "check_normality_detrend",
    fig = plot(norm_result, detrend = TRUE)
  )
})


test_that("plot.see_check_homogeneity() renders correctly", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  set.seed(123)
  model <- lm(mpg ~ wt, data = mtcars)

  # Test homogeneity with default theme
  expect_doppelganger_with_seed(
    title = "check_homogeneity_default",
    fig = plot(performance::check_model(model, check = "homogeneity"))
  )

  # Test with modern theme
  expect_doppelganger_with_seed(
    title = "check_homogeneity_modern",
    fig = plot(performance::check_model(model, check = "homogeneity")) +
      theme_modern()
  )
})


test_that("plot.see_check_outliers() renders correctly", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  set.seed(123)
  model <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

  # Test outliers with default theme
  expect_doppelganger_with_seed(
    title = "check_outliers_default",
    fig = plot(performance::check_model(model, check = "outliers"))
  )

  # Test with modern theme
  expect_doppelganger_with_seed(
    title = "check_outliers_modern",
    fig = plot(performance::check_model(model, check = "outliers")) +
      theme_modern()
  )
})


test_that("plot.see_check_model() handles large datasets efficiently", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  # Test that sampling produces consistent plots with larger datasets
  set.seed(123)
  large_data <- data.frame(
    x = rnorm(5000),
    y = rnorm(5000) + 0.3 * rnorm(5000),
    z = rnorm(5000)
  )
  model_large <- lm(y ~ x + z, data = large_data)

  # Test linearity with large dataset
  expect_doppelganger_with_seed(
    title = "check_model_large_linearity",
    fig = plot(performance::check_model(model_large, check = "linearity"))
  )

  # Test QQ plot with large dataset
  expect_doppelganger_with_seed(
    title = "check_model_large_qq",
    fig = plot(performance::check_model(model_large, check = "qq"))
  )
})


test_that("plot.see_check_model() works with show_dots parameter", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  set.seed(123)
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Test without dots
  expect_doppelganger_with_seed(
    title = "check_model_no_dots",
    fig = plot(performance::check_model(
      model,
      check = "linearity",
      show_dots = FALSE
    ))
  )

  # Test with dots
  expect_doppelganger_with_seed(
    title = "check_model_with_dots",
    fig = plot(performance::check_model(
      model,
      check = "linearity",
      show_dots = TRUE
    ))
  )
})
