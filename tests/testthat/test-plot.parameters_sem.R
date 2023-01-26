test_that("`plot.see_parameters_model()` works", {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- model_parameters(m)

  expect_s3_class(plot(result), "gg")
})
