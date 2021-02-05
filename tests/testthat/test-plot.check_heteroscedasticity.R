test_that("`plot.see_check_heteroscedasticity()` works", {
  library(performance)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- check_heteroscedasticity(m)

  expect_s3_class(plot(result, data = m), "gg")
})
