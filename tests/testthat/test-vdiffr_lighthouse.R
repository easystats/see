skip_if_not_installed("vdiffr")
skip_if_not_installed("modelbased")
skip_if_not_installed("marginaleffects")

test_that("lighthouse plots render correctly", {
  data(iris)

  model <- lm(Sepal.Width ~ Species, data = iris)
  contrasts <- modelbased::estimate_contrasts(model, contrast = "Species")
  means <- modelbased::estimate_means(model, by = "Species")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.estimate_contrasts()` works - lighthouse",
    fig = plot(contrasts, means)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.estimate_contrasts()` works - lighthouse, with data",
    fig = plot(contrasts, means, show_data = TRUE)
  )

  data(mtcars)
  model <- lm(mpg ~ cyl * am, data = mtcars)
  means <- modelbased::estimate_means(model, by = c("cyl", "am"))
  contrasts <- modelbased::estimate_contrasts(
    model,
    contrast = c("am", "cyl")
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.estimate_contrasts()` works - lighthouse multiple",
    fig = plot(contrasts, means)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.estimate_contrasts()` works - lighthouse multiple, with data",
    fig = plot(contrasts, means, show_data = TRUE)
  )
})


test_that("lighthouse for proportions render correctly", {
  data(mtcars)
  model <- glm(vs ~ am, data = mtcars, family = binomial)
  means <- modelbased::estimate_means(model, by = "am")
  contrasts <- modelbased::estimate_contrasts(model, contrast = "am")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.estimate_contrasts()` proportions",
    fig = plot(contrasts, means)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`plot.estimate_contrasts()` proportions, with data",
    fig = plot(contrasts, means, show_data = TRUE)
  )
})
