test_that("`plot.see_cluster_analysis()` works", {
  groups <- parameters::cluster_analysis(iris[, 1:4], 3)
  expect_s3_class(plot(groups), "gg")
})
