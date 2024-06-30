test_that("`check_model()` works with lm", {
  skip_on_cran()
  skip_if_not_installed("merDeriv")

  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  set.seed(123)
  x <- performance::check_model(m)
  # vdiffr test is too fragile for this complex of an output
  expect_s3_class(x, "check_model")
})
