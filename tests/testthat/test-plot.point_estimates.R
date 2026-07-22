test_that("`plot.see_point_estimate()` works", {
  x <- bayestestR::distribution_normal(1000, mean = 0.5, sd = 1)
  result <- bayestestR::point_estimate(x, centrality = c("median", "mean", "MAP"))
  expect_s3_class(plot(result), c("gg", "ggplot"))

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "plot.point_estimate",
    fig = plot(result)
  )
})
