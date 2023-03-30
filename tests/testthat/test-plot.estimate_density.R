test_that("`plot.see_estimate_density()` works", {
  skip_if_not_installed("correlation")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::estimate_density(m_rstan)
  expect_s3_class(plot(result), "gg")
})
