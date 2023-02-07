test_that("`plot.see_p_direction()` works", {
  skip_if_not_or_load_if_installed("rstanarm")
  skip_if_not_or_load_if_installed("ggridges")

  set.seed(123)
  m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
  result <- bayestestR::p_direction(m)

  expect_s3_class(plot(result), "gg")
})
