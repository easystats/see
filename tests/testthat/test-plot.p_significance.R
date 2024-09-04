test_that("`plot.see_p_significance()` works", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::p_significance(m_rstan)

  expect_s3_class(plot(result), "gg")
})

skip_if_not_installed("bayestestR", minimum_version = "0.14.1")

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
