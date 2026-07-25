test_that("`plot.see_check_heteroscedasticity()` works", {
  m <- stats::lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- performance::check_heteroscedasticity(m)

  expect_s3_class(plot(result, data = m), "gg")
})

test_that("`plot.see_check_heteroscedasticity()` snapshot", {
  skip_if_not_installed("vdiffr")

  m <- stats::lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  result <- performance::check_heteroscedasticity(m)
  expect_s3_class(plot(result, data = m), c("gg", "ggplot"))

  vdiffr::expect_doppelganger(
    title = "plot.check_heteroscedasticity",
    fig = plot(result, data = m)
  )
})
