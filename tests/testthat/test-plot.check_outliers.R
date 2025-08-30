test_that("`plot.see_check_outliers()` works", {
  mt1 <- mtcars[, c(1, 3, 4)]
  mt2 <- rbind(
    mt1,
    data.frame(
      mpg = c(37, 40),
      disp = c(300, 400),
      hp = c(110, 120)
    )
  )
  model <<- stats::lm(disp ~ mpg + hp, data = mt2)
  expect_s3_class(
    plot(performance::check_outliers(model, verbose = FALSE)),
    "gg"
  )

  skip_if_not_installed("ggrepel")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works default method",
    fig = plot(performance::check_outliers(model, verbose = FALSE))
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works bars method",
    fig = suppressWarnings(plot(
      performance::check_outliers(model, verbose = FALSE),
      type = "bars"
    ))
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works count method",
    fig = suppressWarningsplot(
      performance::check_outliers(model, verbose = FALSE),
      type = "count"
    )
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works scree method",
    fig = plot(performance::check_outliers(model, verbose = FALSE), type = "scree")
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works scree method, z-score",
    fig = plot(performance::check_outliers(mt2$mpg, method = "zscore"), type = "scree")
  )
})
