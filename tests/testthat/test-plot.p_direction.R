test_that("`plot.see_p_direction()` works", {
  requiet("rstanarm")
  requiet("ggridges")

  set.seed(123)
  m <<- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0)
  result <- bayestestR::p_direction(m)

  expect_s3_class(plot(result), "gg")
})
