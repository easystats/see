test_that("`plot.see_rope()` works", {
  requiet("rstanarm")
  set.seed(123)
  m <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0)
  result <- bayestestR::rope(m)

  expect_s3_class(plot(result), "gg")
})
