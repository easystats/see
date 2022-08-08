if (getRversion() >= "4.1" && require("vdiffr")) {
  # skip_if_not(.Platform$OS.type == "windows")

  test_that("`plot.see_n_factors()` works", {
    if (require("parameters") && require("nFactors")) {
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
    }
  })
}
