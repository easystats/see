test_that("`plot.see_parameters_simulate()` works", {
  m <<- lm(mpg ~ wt + cyl + gear, data = mtcars)
  result <- parameters::simulate_parameters(m)

  expect_s3_class(plot(result), "gg")
})

test_that("`plot.see_parameters_simulate()` snapshot", {
  skip_if_not_installed("vdiffr")

  m <- stats::lm(mpg ~ wt + cyl + gear, data = mtcars)
  set.seed(123)
  result <- parameters::simulate_parameters(m)
  p <- plot(result)
  expect_s3_class(p, c("gg", "ggplot"))

  vdiffr::expect_doppelganger(
    title = "plot.parameters_simulate",
    fig = p
  )
})
