test_that("`plot.see_point_estimate()` works", {
  skip_if_not_or_load_if_installed("rstanarm")
  set.seed(123)
  m <- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
  result <- bayestestR::point_estimate(m, centrality = "median")

  expect_s3_class(plot(result), "ggplot")
})
