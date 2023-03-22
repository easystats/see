test_that("`plot.see_estimate_density()` works", {
  skip_if_not(getRversion() >= "4.1")
  skip_if_not_or_load_if_installed("correlation")
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("ggridges")

  set.seed(123)
  m_rstan <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
  result <- bayestestR::estimate_density(m_rstan)
  expect_s3_class(plot(result), "gg")
})
