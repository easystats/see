test_that("`plot.see_n_factors()` works", {
  skip_if_not_installed("nFactors")

  result <- parameters::n_factors(datasets::mtcars, type = "PCA")

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

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "area graph",
    fig = plot(result, type = "area")
  )
})
