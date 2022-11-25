test_that("`plot.see_estimate_density()` works", {
  skip_if_not(getRversion() >= "4.1")
  requiet("correlation")
  requiet("rstanarm")
  requiet("ggridges")

  set.seed(123)
  m <<- stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0)
  result <- bayestestR::estimate_density(m)
  expect_s3_class(plot(result), "gg")
})
