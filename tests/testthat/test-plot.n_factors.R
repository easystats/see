test_that("`plot.see_n_factors()` works", {
  skip_if_not_or_load_if_installed("parameters")
  skip_if_not_or_load_if_installed("nFactors")
  skip_if_not_or_load_if_installed("vdiffr")
  data(mtcars)
  result <- n_factors(mtcars, type = "PCA")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "bar graph",
    fig = plot(result)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "line graph",
    fig = plot(result, type = "line")
  )
})
