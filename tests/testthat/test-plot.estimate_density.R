test_that("`plot.see_estimate_density()` works", {
  skip_if_not_installed("correlation")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")

  set.seed(123)
  result <- bayestestR::estimate_density(m_rstan)
  expect_s3_class(plot(result), "gg")
})


test_that("`plot.see_estimate_density()`, adding prior layers works", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("ggridges")
  skip_if_not_installed("vdiffr")

  set.seed(123)
  m <<- insight::download_model("stanreg_glm_2")
  skip_if(is.null(m))
  result <- bayestestR::estimate_density(m)
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.estimate_density with piors",
    fig = plot(result, stack = FALSE, priors = TRUE)
  )
})
