test_that("`plot.see_point_estimate()` works", {
  skip_if_not_installed("rstanarm")

  set.seed(123)
  result <- bayestestR::point_estimate(m_rstan, centrality = "median")

  expect_s3_class(plot(result), "ggplot")
})

test_that("`plot.see_point_estimate()` snapshot", {
  x <- bayestestR::distribution_normal(1000, mean = 0.5, sd = 1)
  result <- bayestestR::point_estimate(
    x,
    centrality = c("median", "mean", "MAP")
  )
  expect_s3_class(plot(result), c("gg", "ggplot"))

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title = "plot.point_estimate",
    fig = plot(result)
  )
})

test_that("nearby point estimates have separate label heights", {
  draws <- seq(-2, 2, length.out = 1001)
  result <- bayestestR::point_estimate(draws, centrality = "all")
  built <- ggplot2::ggplot_build(plot(result))
  text <- Filter(function(layer) "label" %in% names(layer), built$data)
  label_x <- vapply(text, function(layer) unique(layer$x), numeric(1))
  label_y <- vapply(text, function(layer) unique(layer$y), numeric(1))

  expect_length(text, 3L)
  expect_equal(label_x[1], label_x[2])
  expect_true(all(diff(label_y) > 0))
  expect_lt(max(label_y), built$layout$panel_params[[1]]$y.range[2])

  hidden <- ggplot2::ggplot_build(plot(result, show_labels = FALSE))
  expect_false(any(vapply(
    hidden$data,
    function(layer) "label" %in% names(layer),
    logical(1)
  )))
})

test_that("`plot.see_point_estimate()` rstanarm snapshot", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  set.seed(123)
  m_stan_dl <<- insight::download_model("stanreg_lm_1")
  skip_if(is.null(m_stan_dl))
  result <- bayestestR::point_estimate(m_stan_dl, centrality = "median")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.point_estimate rstanarm",
    fig = plot(result, data = m_stan_dl)
  )
})
