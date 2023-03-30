test_that("`print.see_performance_pp_check()` works", {
  model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
  result <- performance::check_posterior_predictions(model)
  expect_s3_class(plot(result), "gg")
})
