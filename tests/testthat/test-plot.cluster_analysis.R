if (require("parameters") && require("see") && utils::packageVersion("parameters") > "0.18.1") {
  test_that("`plot.see_cluster_analysis()` works", {
    groups <- cluster_analysis(iris[, 1:4], 3)
    expect_s3_class(plot(groups), "gg")
  })
}
