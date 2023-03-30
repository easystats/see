test_that("`plot.see_rope()` works", {
  skip_if_not_installed("rstanarm")

  set.seed(123)
  result <- bayestestR::rope(m_rstan, verbose = FALSE)

  expect_s3_class(plot(result), "gg")
})
