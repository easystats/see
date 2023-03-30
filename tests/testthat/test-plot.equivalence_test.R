test_that("`plot.see_equivalence_test()` works", {
  skip_if_not_installed("ggridges")
  m_aov <- stats::aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result_ez <- effectsize::eta_squared(m_aov, verbose = FALSE)
  expect_s3_class(plot(result_ez), "gg")
})
