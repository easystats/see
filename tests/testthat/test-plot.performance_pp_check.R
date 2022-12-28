test_that("`print.see_performance_pp_check()` works", {
  requiet("performance")
  model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length,
    data = iris
  )

  expect_s3_class(plot(check_posterior_predictions(model)), "gg")
})
