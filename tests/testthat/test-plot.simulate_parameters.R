skip("Fails at retrieving data")

skip_on_cran()
skip_if_offline()
skip_if_not_installed("mclogit")
skip_if_not_installed("parameters")


test_that("`plot()` for simulate_parameters", {
  pict <- base::readRDS(url("https://slcladal.github.io/data/pict.rda", "rb"))
  d <<- pict
  suppressWarnings({
    m1.mn <- mclogit::mblogit(
      formula = Response ~ Gender + Group,
      random = ~ 1 | Item,
      data = d
    )
  })
  set.seed(1234)
  ms <- parameters::simulate_parameters(m1.mn)
  set.seed(1234)
  vdiffr::expect_doppelganger(
    title = "plot.simulate_parameters works",
    fig = plot(ms)
  )
  set.seed(1234)
  vdiffr::expect_doppelganger(
    title = "plot.simulate_parameters works-2",
    fig = plot(ms, stack = FALSE)
  )
  set.seed(1234)
  vdiffr::expect_doppelganger(
    title = "plot.simulate_parameters works-3",
    fig = plot(
      ms,
      stack = FALSE,
      show_intercept = TRUE,
      normalize_height = TRUE
    )
  )
  set.seed(1234)
  vdiffr::expect_doppelganger(
    title = "plot.simulate_parameters works-4",
    fig = plot(ms, stack = TRUE, show_intercept = TRUE, normalize_height = TRUE)
  )
})
