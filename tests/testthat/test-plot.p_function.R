test_that("`plot.see_p_function()` works", {
  m <- lm(mpg ~ wt + cyl, data = mtcars)
  result <- parameters::p_function(m)
  expect_s3_class(plot(result), c("gg", "ggplot"))

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "plot.p_function",
    fig = plot(result)
  )
})
