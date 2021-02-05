test_that("`plot.see_estimate_contrasts()` works", {
  if (require("modelbased") && require("rstanarm")) {
    model <- stan_glm(Sepal.Width ~ Species, data = iris, refresh = 0)
    contrasts <- estimate_contrasts(model)
    means <- estimate_means(model)
    expect_s3_class(plot(contrasts, means), "gg")
  }
})
