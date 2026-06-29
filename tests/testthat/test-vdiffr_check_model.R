skip_on_cran()
skip_if_not_installed("vdiffr")
skip_if_not_installed("performance")

# Snapshot tests for check_model() plots
# These tests verify that diagnostic plots render consistently across versions.
# Related to #236 (snapshot tests) and #420 (large dataset performance).

# Helper function for reproducible vdiffr tests
expect_doppelganger_with_seed <- function(title, fig, seed = 123) {
  set.seed(seed)
  vdiffr::expect_doppelganger(title = title, fig = fig)
}

test_that("plot.see_check_model() renders correctly", {
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
  set.seed(123)
  data(mtcars)
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

  # Test with modern theme
  expect_doppelganger_with_seed(
    title = "check_model_theme_modern_2",
    fig = plot(performance::check_model(model, theme = theme_modern()))
  )
})


test_that("plot.see_check_model() works with custom colors", {
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


test_that("plot outliers skips smooth for low N", {
  # fmt: skip
  sdata <- data.frame(
    TrtLin = factor(rep(
      c("C1", "C2", "C3", "C4", "S1", "S2", "S3", "S4"),
      rep(c(35L, 63L, 62L, 57L), 2)
    )),
    this_male_mated = rep(
      c(rep(0:1, 98), 0),
      c(
        2L, 2L, 6L, 4L, 2L, 1L, 3L, 2L, 1L, 3L, 3L, 1L, 5L, 1L, 1L, 1L, 1L, 1L, 2L,
        1L, 4L, 2L, 3L, 2L, 1L, 11L, 2L, 2L, 1L, 4L, 3L, 6L, 1L, 1L, 4L, 3L, 2L, 1L,
        7L, 2L, 3L, 2L, 2L, 5L, 2L, 1L, 3L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 3L, 1L, 2L,
        1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 4L, 2L, 2L, 2L, 1L, 1L, 4L, 1L, 1L, 1L,
        1L, 1L, 3L, 1L, 9L, 6L, 1L, 5L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L,
        2L, 2L, 2L, 1L, 2L, 1L, 7L, 1L, 1L, 2L, 8L, 1L, 2L, 1L, 6L, 1L, 4L, 1L, 1L,
        3L, 3L, 2L, 2L, 1L, 1L, 3L, 1L, 2L, 1L, 1L, 1L, 2L, 8L, 3L, 2L, 3L, 3L, 1L,
        4L, 1L, 2L, 3L, 3L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 3L, 5L,
        1L, 2L, 2L, 2L, 3L, 1L, 3L, 4L, 3L, 2L, 1L, 5L, 4L, 1L, 1L, 3L, 1L, 3L, 1L,
        1L, 1L, 1L, 1L, 3L, 2L, 1L, 3L, 3L, 5L, 1L, 1L, 3L, 1L, 2L, 1L, 2L, 2L, 3L,
        3L, 2L, 2L, 1L, 2L, 6L, 1L
      )
    )
  )

  themodel <- glm(this_male_mated ~ TrtLin, data = sdata, family = binomial)

  expect_doppelganger_with_seed(
    title = "outliers_plot_low_N",
    fig = plot(performance::check_model(themodel))
  )
})


test_that("ppc_range works", {
  skip_if_not_installed("MASS")
  set.seed(3)
  mu <- rpois(500, lambda = 3)
  x <- pmax(ceiling(rnorm(500, mu, mu * 3)), 0)
  quine.nb1 <- MASS::glm.nb(x ~ mu)

  expect_doppelganger_with_seed(
    title = "ppc_range_works-1",
    fig = plot(performance::check_model(quine.nb1, ppc_range = c(0, 10)))
  )

  expect_doppelganger_with_seed(
    title = "ppc_range_works-2",
    fig = plot(performance::check_model(quine.nb1, x_limits = c(0, 10)))
  )
})
