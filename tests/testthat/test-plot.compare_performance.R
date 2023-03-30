test_that("`plot.see_compare_performance()` works", {
  lm1 <- stats::lm(Sepal.Length ~ Species, data = iris)
  lm2 <- stats::lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  lm3 <- stats::lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  result <- performance::compare_performance(lm1, lm2, lm3)

  expect_s3_class(plot(result), "gg")
})
