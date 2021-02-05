test_that("`plot.see_bayesfactor_models()` works", {
  library(bayestestR)
  library(see)

  lm0 <- lm(qsec ~ 1, data = mtcars)
  lm1 <- lm(qsec ~ drat, data = mtcars)
  lm2 <- lm(qsec ~ wt, data = mtcars)
  lm3 <- lm(qsec ~ drat + wt, data = mtcars)
  result <- bayesfactor_models(lm1, lm2, lm3, denominator = lm0)

  expect_s3_class(plot(result, n_pies = "one", value = "probability", sort = TRUE) +
    scale_fill_pizza(reverse = TRUE), "gg")

  expect_s3_class(
    plot(result, n_pies = "many", value = "BF", log = TRUE) + scale_fill_pizza(reverse = FALSE),
    "gg"
  )
})
