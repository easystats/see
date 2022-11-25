test_that("`plot.see_n_factors()` works", {
  skip_if_not(getRversion() >= "4.1")
  requiet("parameters")
  requiet("nFactors")
  requiet("vdiffr")
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
