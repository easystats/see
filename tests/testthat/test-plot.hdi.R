test_that("`plot.see_hdi()` works", {
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("ggridges")

  set.seed(123)
  result <- bayestestR::hdi(m_rstan)

  expect_s3_class(plot(result), "gg")
})
