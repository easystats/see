test_that("`plot.see_cluster_analysis()` works", {
  groups <- parameters::cluster_analysis(iris[, 1:4], 3)
  expect_s3_class(plot(groups), "gg")
})

test_that("`plot.see_cluster_analysis()` snapshot", {
  skip_if_not_installed("vdiffr")

  set.seed(123)
  groups <- parameters::cluster_analysis(iris[, 1:4], 3)
  p <- plot(groups)
  expect_s3_class(p, c("gg", "ggplot"))

  vdiffr::expect_doppelganger(
    title = "plot.cluster_analysis",
    fig = p
  )
})
