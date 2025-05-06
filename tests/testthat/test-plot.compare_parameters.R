test_that("`plot()` for compare_parameters", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  skip_if_not_installed("parameters")
  gdat <- readRDS(system.file(
    "vignette_data",
    "gophertortoise.rds",
    package = "glmmTMB"
  ))
  form <- shells ~ prev + offset(log(Area)) + factor(year) + (1 | Site)
  gmod_glmer <- lme4::glmer(form, family = poisson, data = gdat)
  gprior <- data.frame(
    prior = "gamma(1e8, 2.5)",
    class = "theta",
    coef = "",
    stringsAsFactors = FALSE
  )
  gmod_glmmTMB <- glmmTMB::glmmTMB(
    form,
    family = poisson,
    priors = gprior,
    data = gdat
  )

  cp <- parameters::compare_parameters(
    gmod_glmer,
    gmod_glmmTMB,
    effects = "random"
  )
  expect_warning(plot(cp), "No data left")

  vdiffr::expect_doppelganger(
    title = "plot.compare_parameters works",
    fig = plot(cp, show_intercept = TRUE)
  )
})
