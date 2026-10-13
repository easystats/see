test_that("lighthouse plots render correctly", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("modelbased")
  skip_if_not_installed("marginaleffects")

  data(iris)

  model <- lm(Sepal.Width ~ Species, data = iris)
  contrasts <- modelbased::estimate_contrasts(model, contrast = "Species")
  means <- modelbased::estimate_means(model, by = "Species")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.estimate_contrasts()` works - lighthouse",
    fig = plot(contrasts, means)
  )
})
