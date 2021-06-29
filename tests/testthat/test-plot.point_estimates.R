test_that("`plot.see_point_estimate()` works", {
  if (require("bayestestR") && require("rstanarm")) {
    set.seed(123)
    m <- stan_glm(Sepal.Length ~ Petal.Width * Species,
      data = iris,
      refresh = 0
    )
    result <- point_estimate(m, centrality = "median")

    expect_s3_class(plot(result), "ggplot")
  }
})
