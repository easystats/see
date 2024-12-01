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


test_that("`plot()` for check_dag, multiple adjustment sets", {
  set.seed(1234)
  dag <- performance::check_dag(
    podcast ~ mood + humor + skills_course,
    alertness ~ mood,
    mood ~ humor,
    prepared ~ skills_course,
    exam ~ alertness + prepared,
    exposure = "podcast",
    outcome = "exam",
    coords = ggdag::time_ordered_coords()
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag multiple adjustments",
    fig = plot(dag)
  )
})


test_that("`plot()` for check_dag, different total and direct adjustments", {
  set.seed(1234)
  dag <- performance::check_dag(
    outcome ~ exposure + x1 + x2,
    x2 ~ exposure,
    exposure ~ x1,
    outcome = "outcome",
    exposure = "exposure",
    coords = data.frame(
      name = c("outcome", "exposure", "x1", "x2"),
      x = c(0, 1, 0, 0.5),
      y = c(0, 0.5, 1, 1),
      stringsAsFactors = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag total1",
    fig = plot(dag)
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag direct1",
    fig = plot(plot(dag, effect = "direct"))
  )

  dag <- performance::check_dag(
    outcome ~ exposure + x1 + x2,
    x2 ~ exposure,
    exposure ~ x1,
    adjusted = "x1",
    outcome = "outcome",
    exposure = "exposure",
    coords = data.frame(
      name = c("outcome", "exposure", "x1", "x2"),
      x = c(0, 1, 0, 0.5),
      y = c(0, 0.5, 1, 1),
      stringsAsFactors = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag total2",
    fig = plot(dag)
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag direct2",
    fig = plot(plot(dag, effect = "direct"))
  )

  dag <- performance::check_dag(
    outcome ~ exposure + x1 + x2,
    x2 ~ exposure,
    exposure ~ x1,
    adjusted = "x2",
    outcome = "outcome",
    exposure = "exposure",
    coords = data.frame(
      name = c("outcome", "exposure", "x1", "x2"),
      x = c(0, 1, 0, 0.5),
      y = c(0, 0.5, 1, 1),
      stringsAsFactors = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag total3",
    fig = plot(dag)
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag direct3",
    fig = plot(plot(dag, effect = "direct"))
  )

  dag <- performance::check_dag(
    outcome ~ exposure + x1 + x2,
    x2 ~ exposure,
    exposure ~ x1,
    adjusted = c("x1", "x2"),
    outcome = "outcome",
    exposure = "exposure",
    coords = data.frame(
      name = c("outcome", "exposure", "x1", "x2"),
      x = c(0, 1, 0, 0.5),
      y = c(0, 0.5, 1, 1),
      stringsAsFactors = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag total4",
    fig = plot(dag)
  )
  vdiffr::expect_doppelganger(
    title = "plot.check_dag direct4",
    fig = plot(plot(dag, effect = "direct"))
  )
})
