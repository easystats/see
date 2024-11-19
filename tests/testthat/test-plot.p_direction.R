test_that("`plot.see_p_direction()` works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::p_direction(m_rstan)

  expect_s3_class(plot(result), "gg")
})

skip_on_cran()
skip_if_not_installed("bayestestR", minimum_version = "0.14.1")
skip_if_not_installed("parameters", minimum_version = "0.22.3")

test_that("`plot.see_p_direction` works {parameters}", {
  skip_if_not_installed("ggridges")

  data(qol_cancer, package = "parameters")
  model <- lm(QoL ~ time + age + education, data = qol_cancer)
  set.seed(123)
  out <- parameters::p_direction(model)
  vdiffr::expect_doppelganger(
    title = "plot.p_dir_frequ1",
    fig = plot(out)
  )
  set.seed(123)
  out <- parameters::p_direction(model, null = 2)
  vdiffr::expect_doppelganger(
    title = "plot.p_dir_frequ2",
    fig = plot(out)
  )
})

test_that("plot p_direction, glmmTMB", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("ggridges")

  data(Salamanders, package = "glmmTMB")
  m1 <- glmmTMB::glmmTMB(count ~ mined + cover + (1 | site),
    zi = ~mined,
    family = poisson,
    data = Salamanders
  )
  set.seed(123)
  out <- parameters::p_direction(m1)
  vdiffr::expect_doppelganger(
    title = "plot.p_dir_glmmTMB",
    fig = plot(out)
  )
})
