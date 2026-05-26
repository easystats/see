skip_on_cran()

# Snapshot tests for check_model() plots
# These tests verify that diagnostic plots render consistently across versions.
# Related to #236 (snapshot tests) and #420 (large dataset performance).

# Helper function for reproducible vdiffr tests
expect_doppelganger_with_seed <- function(title, fig, seed = 123) {
  set.seed(seed)
  vdiffr::expect_doppelganger(title = title, fig = fig)
}

test_that("plot.see_check_predictions() renders correctly", {
  skip_if_not_installed("performance")
  skip_if_not_installed("see")
  skip_on_cran()

  data(mtcars)
  model <- lm(mpg ~ disp, data = mtcars)

  expect_doppelganger_with_seed(
    title = "check_predictions_no_range",
    fig = plot(performance::check_predictions(model))
  )

  expect_doppelganger_with_seed(
    title = "check_predictions_with_range",
    fig = plot(performance::check_predictions(model, check_range = TRUE))
  )
})


test_that("check_predicions - type and x_limits arguments work", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("performance", minimum_version = "0.17.0.3")
  set.seed(3)
  mu <- rpois(500, lambda = 3)
  x <- pmax(ceiling(rnorm(500, mu, mu * 3)), 0)
  quine.nb1 <- MASS::glm.nb(x ~ mu)

  expect_doppelganger_with_seed(
    title = "check_predicions_type_1",
    fig = plot(performance::check_predictions(quine.nb1))
  )

  expect_doppelganger_with_seed(
    title = "check_predicions_type_2",
    fig = plot(performance::check_predictions(
      quine.nb1,
      type = "discrete_dots"
    ))
  )

  expect_doppelganger_with_seed(
    title = "check_predicions_xlim_1",
    fig = plot(performance::check_predictions(
      quine.nb1,
      x_limits = c(0, 20)
    ))
  )
})
