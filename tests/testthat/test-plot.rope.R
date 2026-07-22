test_that("`plot.see_rope()` works", {
  x <- bayestestR::distribution_normal(1000, mean = 0.5, sd = 1)
  result <- bayestestR::rope(x, range = c(-0.1, 0.1))
  expect_s3_class(plot(result), c("gg", "ggplot"))

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "plot.rope",
    fig = plot(result)
  )
})
