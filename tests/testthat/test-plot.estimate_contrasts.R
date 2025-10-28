test_that("`plot.see_estimate_contrasts()` works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("collapse")

  model <- stan_glm(Sepal.Width ~ Species, data = iris, refresh = 0)
  contrasts <- modelbased::estimate_contrasts(model, contrast = "Species")
  means <- modelbased::estimate_means(model, by = "Species")
  expect_s3_class(plot(contrasts, means), "gg")
})
