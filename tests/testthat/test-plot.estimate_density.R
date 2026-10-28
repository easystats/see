test_that("`plot.see_estimate_density()` works", {
  skip_if_not_installed("correlation")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::estimate_density(m_rstan)
  expect_s3_class(plot(result), "gg")
})


test_that("`plot.see_estimate_density()` works with group_by and vector input", {
  skip_if_not_installed("bayestestR")
  skip_if_not_installed("vdiffr")
  # Test case that was failing: vector input with group_by
  df <- bayestestR::estimate_density(
    iris[c("Species", "Petal.Width")],
    group_by = "Species"
  )

  # This should not error
  expect_no_error(p <- plot(df))
  expect_s3_class(p, "gg")
  # Visual snapshot test
  vdiffr::expect_doppelganger(
    title = "plot.estimate_density with group_by and vector input",
    fig = plot(df)
  )
})


test_that("`plot.see_estimate_density()`, adding prior layers works", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")
  skip_if_not_installed("vdiffr")

  set.seed(123)
  m <<- insight::download_model("stanreg_glm_2")
  skip_if(is.null(m))
  result <- bayestestR::estimate_density(m)
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.estimate_density with piors",
    fig = plot(result, stack = FALSE, priors = TRUE)
  )
})


test_that("`plot.see_estimate_density()` prior layer keeps all rows of several parameters", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::estimate_density(
    m_rstan,
    parameters = c("Petal.Width", "Speciesversicolor")
  )
  prior_dens <- bayestestR::estimate_density(bayestestR::simulate_prior(m_rstan))
  n_grid <- sum(prior_dens$Parameter == "Petal.Width")

  # `==` recycled the parameter names and kept only some prior rows (#494)
  expect_no_warning(p <- plot(result, stack = FALSE, priors = TRUE))
  prior_rows <- ggplot2::layer_data(p, 1)
  expect_identical(
    as.vector(table(prior_rows$group)),
    rep(n_grid, length(unique(result$Parameter)))
  )
})
