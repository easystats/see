test_that("`plot.see_estimate_contrasts()` works", {
  requiet("modelbased")
  requiet("rstanarm")
  requiet("emmeans")
  skip_if_not(getRversion() >= "4.1")

  model <- rstanarm::stan_glm(Sepal.Width ~ Species, data = iris, refresh = 0)
  contrasts <- modelbased::estimate_contrasts(model)
  means <- modelbased::estimate_means(model)
  expect_s3_class(plot(contrasts, means), "gg")
})
