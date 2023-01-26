test_that("`print.see_performance_pp_check()` works", {
  skip_if_not_or_load_if_installed("performance")
  model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length,
    data = iris
  )

  expect_s3_class(plot(check_posterior_predictions(model)), "gg")
})
