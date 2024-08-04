skip_if_not_installed("vdiffr")
skip_if_not_installed("ggdag")
skip_if_not_installed("dagitty")

test_that("`plot()` for check_dag", {
  set.seed(1234)
  dag <- performance::check_dag(
    y ~ x + b + c,
    x ~ b,
    outcome = "y",
    exposure = "x",
    coords = list(
      x = c(y = 5, x = 4, b = 3, c = 3),
      y = c(y = 3, x = 3, b = 2, c = 4)
    )
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag all",
    fig = plot(dag)
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag current",
    fig = plot(dag, which = "current")
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag required",
    fig = plot(dag, which = "required")
  )
  set.seed(1234)
  dag <- performance::check_dag(
    y ~ x + b + c,
    x ~ b,
    outcome = "y",
    exposure = "x",
    adjusted = "b",
    coords = list(
      x = c(y = 5, x = 4, b = 3, c = 3),
      y = c(y = 3, x = 3, b = 2, c = 4)
    )
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag all-adjusted",
    fig = plot(dag)
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag current-adjusted",
    fig = plot(dag, which = "current")
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag required-adjusted",
    fig = plot(dag, which = "required")
  )
})
