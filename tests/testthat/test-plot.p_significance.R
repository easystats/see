test_that("`plot.see_p_significance()` works", {
  requiet("rstanarm")
  requiet("ggridges")

  set.seed(123)
  m <<- stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0)
  result <- bayestestR::p_significance(m)

  expect_s3_class(plot(result), "gg")
})
