test_that("`plot.see_check_heteroscedasticity()` works", {
  m <- stats::lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- performance::check_heteroscedasticity(m)

  expect_s3_class(plot(result, data = m), "gg")
})
