test_that("`plot.see_check_collinearity()` works", {
  library(performance)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- check_collinearity(m)

  expect_s3_class(plot(result), "gg")
})
