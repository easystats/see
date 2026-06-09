skip_on_cran()

# Helper function for reproducible vdiffr tests
expect_doppelganger_with_seed <- function(title, fig, seed = 123) {
  set.seed(seed)
  vdiffr::expect_doppelganger(title = title, fig = fig)
}

test_that("plot.see_check_priors() renders correctly", {
  skip_if_not_installed("performance")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  set.seed(333)
  model <- insight::download_model("stan_prior_checks_1")
  skip_if(is.null(model))
  expect_doppelganger_with_seed(
    title = "check_priors_believer",
    fig = plot(performance::check_priors(model, "mmse"))
  )

  expect_doppelganger_with_seed(
    title = "check_priors_believer, continuous",
    fig = plot(performance::check_priors(model, "age"))
  )

  set.seed(333)
  model <- insight::download_model("stan_prior_checks_2")
  skip_if(is.null(model))
  expect_doppelganger_with_seed(
    title = "check_priors_weakly",
    fig = plot(performance::check_priors(model, "mmse"))
  )

  expect_doppelganger_with_seed(
    title = "check_priors_weakly, continuous",
    fig = plot(performance::check_priors(model, "age"))
  )

  expect_error(
    performance::check_priors(model),
    regex = "Argument `predictors` is required",
    fixed = TRUE
  )
})
