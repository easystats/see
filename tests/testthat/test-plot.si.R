test_that("`plot.see_si()` works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("logspline")

  set.seed(123)
  result <- bayestestR::si(m_rstan, verbose = FALSE)

  expect_s3_class(plot(result), "gg")
})
