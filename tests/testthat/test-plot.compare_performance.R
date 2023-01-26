test_that("`plot.see_compare_performance()` works", {
  lm1 <- lm(Sepal.Length ~ Species, data = iris)
  lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  result <- compare_performance(lm1, lm2, lm3)

  expect_s3_class(plot(result), "gg")
})
