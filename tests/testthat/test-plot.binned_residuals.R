test_that("`plot.see_binned_residuals()` works", {
  m <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  set.seed(123)
  result <- performance::binned_residuals(m)
  expect_s3_class(plot(result), c("gg", "ggplot"))

  skip_if_not_installed("vdiffr")
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.binned_residuals",
    fig = plot(result)
  )
})
