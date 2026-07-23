test_that("`plot.see_rope()` works", {
  skip_if_not_installed("rstanarm")

  set.seed(123)
  result <- bayestestR::rope(m_rstan, verbose = FALSE)

  expect_s3_class(plot(result), "gg")
})

test_that("`plot.see_rope()` snapshot", {
  x <- bayestestR::distribution_normal(1000, mean = 0.5, sd = 1)
  result <- bayestestR::rope(x, range = c(-0.1, 0.1))
  expect_s3_class(plot(result), c("gg", "ggplot"))

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "plot.rope",
    fig = plot(result)
  )
})

test_that("`plot.see_rope()` rstanarm snapshot", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  set.seed(123)
  model <- insight::download_model("stanreg_lm_1")
  skip_if(is.null(model))
  result <- bayestestR::rope(model, verbose = FALSE)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.rope rstanarm",
    fig = plot(result, data = model)
  )
})
