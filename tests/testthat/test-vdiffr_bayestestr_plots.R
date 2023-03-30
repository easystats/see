test_that("bayestestR package plots rendered correctly", {
  # plot.see_bayesfactor_models() --------------------

  lm0 <- stats::lm(qsec ~ 1, data = mtcars)
  lm1 <- stats::lm(qsec ~ drat, data = mtcars)
  lm2 <- stats::lm(qsec ~ wt, data = mtcars)
  lm3 <- stats::lm(qsec ~ drat + wt, data = mtcars)
  result <- bayestestR::bayesfactor_models(lm1, lm2, lm3, denominator = lm0)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.see_bayesfactor_models()` works - 1",
    fig = plot(result, n_pies = "one", value = "probability", sort = TRUE) +
      scale_fill_pizza(reverse = TRUE)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.see_bayesfactor_models()` works - 2",
    fig = plot(result, n_pies = "many", value = "BF", log = TRUE) +
      scale_fill_pizza(reverse = FALSE)
  )
})
