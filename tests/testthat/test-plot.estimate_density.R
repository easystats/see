test_that("`plot.see_estimate_density()` works", {
  skip_if_not_installed("correlation")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::estimate_density(m_rstan)
  expect_s3_class(plot(result), "gg")
})


test_that("`plot.see_estimate_density()`, adding prior layers works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")
  skip_if_not_installed("vdiffr")

  set.seed(123)
  result <- bayestestR::estimate_density(m_rstan)
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.estimate_density with piors",
    fig = plot(result, stack = FALSE, priors = TRUE)
  )
})
