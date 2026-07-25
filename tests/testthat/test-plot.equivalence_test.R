test_that("`plot.see_equivalence_test()` works", {
  skip_if_not_installed("ggridges")
  m_aov <- stats::aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result_ez <- effectsize::eta_squared(m_aov, verbose = FALSE)
  expect_s3_class(plot(result_ez), "gg")
})

test_that("`plot.see_equivalence_test()` snapshot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggridges")

  m_aov <- stats::aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result_ez <- effectsize::eta_squared(m_aov, verbose = FALSE)
  p <- plot(result_ez)
  expect_s3_class(p, c("gg", "ggplot"))

  vdiffr::expect_doppelganger(
    title = "plot.equivalence_test",
    fig = p
  )
})
