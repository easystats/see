test_that("`plot.see_si()` works", {
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("logspline")

  set.seed(123)
  m <- stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0)
  result <- bayestestR::si(m)

  expect_s3_class(plot(result), "gg")
})
