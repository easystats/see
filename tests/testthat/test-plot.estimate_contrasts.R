test_that("`plot.see_estimate_contrasts()` works", {
  skip_if_not_installed("modelbased")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("emmeans")

    model <- stan_glm(Sepal.Width ~ Species, data = iris, refresh = 0)
    contrasts <- estimate_contrasts(model)
    means <- estimate_means(model)
    expect_s3_class(plot(contrasts, means), "gg")
})
