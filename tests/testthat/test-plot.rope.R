test_that("`plot.see_rope()` works", {
  skip_if_not_or_load_if_installed("rstanarm")

  set.seed(123)
  result <- bayestestR::rope(m_rstan)

  expect_s3_class(plot(result), "gg")
})
