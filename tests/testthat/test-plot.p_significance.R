test_that("`plot.see_p_significance()` works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::p_significance(m_rstan)

  expect_s3_class(plot(result), "gg")
})

# skip_if_not_installed("bayestestR", minimum_version = "0.14.1")
# skip_if_not_installed("parameters", minimum_version = "0.22.3")

test_that("`plot.see_p_significance works for two thresholds", {
  skip_if_not_installed("vdiffr")
  set.seed(123)
  x <- rnorm(1000, 1, 1.2)
  out <- bayestestR::p_significance(x)
  vdiffr::expect_doppelganger(
    title = "plot.p_sig_simple_threshold",
    fig = plot(out)
  )
  out <- bayestestR::p_significance(x, threshold = c(-0.2, 0.5))
  vdiffr::expect_doppelganger(
    title = "plot.p_sig_threshold_2",
    fig = plot(out)
  )
})

test_that("`plot.see_p_significance works {parameters}}", {
  skip_if_not_installed("vdiffr")
  data(qol_cancer, package = "parameters")
  model <- lm(QoL ~ time + age + education, data = qol_cancer)
  set.seed(123)
  out <- parameters::p_significance(model)
  vdiffr::expect_doppelganger(
    title = "plot.p_sig_frequ1",
    fig = plot(out)
  )
  set.seed(123)
  out <- parameters::p_significance(model, threshold = c(-0.5, 3.3))
  vdiffr::expect_doppelganger(
    title = "plot.p_sig_frequ2",
    fig = plot(out)
  )
  set.seed(123)
  out <- parameters::p_significance(model, threshold = c(-0.5, 5))
  vdiffr::expect_doppelganger(
    title = "plot.p_sig_frequ3",
    fig = plot(out)
  )
})

test_that("plot p_significance, glmmTMB", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  m1 <- glmmTMB::glmmTMB(count ~ mined + cover + (1 | site),
    zi = ~mined,
    family = poisson,
    data = Salamanders
  )
  set.seed(123)
  out <- parameters::p_significance(m1)
  vdiffr::expect_doppelganger(
    title = "plot.p_sig_glmmTMB",
    fig = plot(out)
  )
})
