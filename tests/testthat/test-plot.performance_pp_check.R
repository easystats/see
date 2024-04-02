test_that("`print.see_performance_pp_check()` works", {
  set.seed(123)
  model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
  result <- performance::check_predictions(model)

  vdiffr::expect_doppelganger(
    title = "pp check - lm",
    fig = plot(result)
  )
})
