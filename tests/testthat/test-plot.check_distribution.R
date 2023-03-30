test_that("`plot.see_check_distribution()` works", {
  skip_if_not_installed("randomForest")

  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- check_distribution(m)

  expect_s3_class(plot(result), "gg")
})
