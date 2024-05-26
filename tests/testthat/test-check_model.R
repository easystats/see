test_that("`check_model()` works with lm", {
  skip_on_cran()
  skip_if_not_installed("merDeriv")

  data(mtcars)
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  set.seed(123)
  x <- performance::check_model(m)
  vdiffr::expect_doppelganger(
    title = "check_model works for lm",
    fig = plot(x)
  )
})
