skip_on_cran()
skip_if_not_installed("performance")
skip_if_not_installed("ggplot2")

expect_doppelganger_with_seed <- function(title, fig, seed = 123) {
  set.seed(seed)
  vdiffr::expect_doppelganger(title = title, fig = fig)
}

# ==============================================================================
# ISSUE #851: theme argument of performance::check_model() has no effect
# ==============================================================================
#
# These tests document the expected behavior of the theme argument in
# performance::check_model(). Currently, these tests FAIL due to bugs in the see package's
# plot.performance::check_model() function.
#
# Three main problems identified:
# 1. Standard ggplot2 themes cause "unused arguments" errors
# 2. Custom user-defined theme functions cannot be accessed
# 3. Theme attribute not properly read by see package's plot method
#
# The actual fix needs to be implemented in the see package's
# R/plot.performance::check_model.R file (around lines 64-67).
#
# See: https://github.com/easystats/performance/issues/851
# ==============================================================================

# Test 1: Standard ggplot2 themes should work without errors
# -----------------------------------------------------------------------------
# PROBLEM: Currently throws "unused arguments" error because see package passes
# hardcoded arguments (plot.title.space, axis.title.space, etc.) that don't
# exist in standard ggplot2 themes.
test_that("performance::check_model accepts standard ggplot2 themes as functions", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

  # Test with theme_dark passed as function (not string)
  # Explicit namespace call for clarity and to avoid conflicts
  p1 <- performance::check_model(m, theme = ggplot2::theme_dark())
  # The plot method should handle the theme attribute correctly
  expect_doppelganger_with_seed(
    title = "check_model-theme_attr",
    fig = plot(p1)
  )

  # Test with theme_minimal
  p2 <- performance::check_model(m, theme = ggplot2::theme_minimal())
  expect_doppelganger_with_seed(
    title = "check_model-theme_minimal",
    fig = plot(p2)
  )

  # Test with theme_bw
  p3 <- performance::check_model(m, theme = ggplot2::theme_bw())
  expect_doppelganger_with_seed(
    title = "check_model-theme_bw",
    fig = plot(p3)
  )
})


# Test 2: Theme as string should work for backward compatibility
# -----------------------------------------------------------------------------
# PROBLEM: String parsing doesn't work reliably, and when it does parse,
# it still hits the "unused arguments" error from hardcoded parameters.
test_that("performance::check_model accepts theme as string (backward compatibility)", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

  # String reference should work
  p1 <- performance::check_model(m, theme = "ggplot2::theme_dark")
  expect_doppelganger_with_seed(
    title = "check_model-theme_dark_string",
    fig = plot(p1)
  )

  # Test with theme_bw as string
  p2 <- performance::check_model(m, theme = "ggplot2::theme_bw")
  expect_doppelganger_with_seed(
    title = "check_model-theme_bw_string",
    fig = plot(p2)
  )
})


# Test 3: Custom theme functions should work
# -----------------------------------------------------------------------------
# PROBLEM: Custom themes defined in user environment can't be accessed because
# the see package's string parsing approach only searches package namespaces.
test_that("performance::check_model accepts custom theme functions from user environment", {
  # Define custom theme in test environment
  my_custom_theme <- function(
    base_size = 11,
    base_family = "",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22
  ) {
    ggplot2::theme_minimal(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        panel.grid.major = ggplot2::element_line(color = "gray80")
      )
  }

  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

  # Custom theme function should work when passed directly
  p1 <- performance::check_model(m, theme = my_custom_theme())
  expect_doppelganger_with_seed(
    title = "check_model-theme_custom",
    fig = plot(p1)
  )
})


# Test 4: Theme attribute should be stored and retrieved correctly
# -----------------------------------------------------------------------------
# PROBLEM: The theme attribute is stored by performance::check_model() but may not be
# properly read by see package's plot method.
test_that("theme attribute is stored and used correctly", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

  # Test with function
  p1 <- performance::check_model(m, theme = ggplot2::theme_dark())

  # Theme should be stored as attribute
  expect_true(!is.null(attr(p1, "theme")))

  # Attribute should contain the theme
  theme_attr <- attr(p1, "theme")
  expect_s3_class(theme_attr, "theme")

  # Theme should be applied when plotting
  expect_doppelganger_with_seed(
    title = "check_model-theme_attr-2",
    fig = plot(p1)
  )

  # Test with string
  p2 <- performance::check_model(m, theme = "ggplot2::theme_minimal")
  expect_true(!is.null(attr(p2, "theme")))
  expect_identical(attr(p2, "theme"), "ggplot2::theme_minimal")

  expect_doppelganger_with_seed(
    title = "check_model-theme_attr-3",
    fig = plot(p2)
  )
})


# Test 5: Default theme should work when no theme specified
# -----------------------------------------------------------------------------
# Tests that performance::check_model works correctly when no theme argument is provided,
# ensuring it falls back to the default theme.
test_that("performance::check_model works without theme argument (default behavior)", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

  # Should work with default theme
  p <- performance::check_model(m)
  expect_doppelganger_with_seed(
    title = "check_model-without_theme",
    fig = plot(p)
  )

  # Default theme should be stored
  p <- performance::check_model(m)
  expect_true(!is.null(attr(p, "theme")))
})


# Test 6: theme argument in plot() should override theme from performance::check_model()
# -----------------------------------------------------------------------------
# This tests that the plot method's theme parameter can override the theme
# set during performance::check_model() call.
test_that("plot theme argument overrides performance::check_model theme", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)

  # Create with one theme
  p <- performance::check_model(m, theme = ggplot2::theme_dark())

  # Plot with different theme - should work
  expect_doppelganger_with_seed(
    title = "check_model-different_theme-1",
    fig = plot(p, theme = ggplot2::theme_minimal())
  )
  expect_doppelganger_with_seed(
    title = "check_model-different_theme-2",
    fig = plot(p, theme = ggplot2::theme_bw())
  )
})
