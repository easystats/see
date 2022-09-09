test_that("`plot.see_estimate_contrasts()` works", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("emmeans")

  model <- rstanarm::stan_glm(Sepal.Width ~ Species, data = iris, refresh = 0)
  contrasts <- modelbased::estimate_contrasts(model)
  means <- modelbased::estimate_means(model)
  expect_s3_class(plot(contrasts, means), "gg")
})
