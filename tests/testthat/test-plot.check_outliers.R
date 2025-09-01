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
  skip_on_os("linux")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works default method",
    fig = plot(performance::check_outliers(model, verbose = FALSE)),
    variant = "windows"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works bars method",
    fig = plot(
      performance::check_outliers(model, verbose = FALSE),
      type = "bars"
    ),
    variant = "windows"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works bars, rescaled",
    fig = plot(
      performance::check_outliers(model, verbose = FALSE),
      type = "bars",
      rescale_distance = TRUE
    ),
    variant = "windows"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works count method",
    fig = suppressMessages(print(plot(
      performance::check_outliers(model, verbose = FALSE),
      type = "count"
    ))),
    variant = "windows"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works scree method",
    fig = plot(
      performance::check_outliers(model, verbose = FALSE),
      type = "scree",
      verbose = FALSE
    ),
    variant = "windows"
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works scree method, z-score",
    fig = plot(
      performance::check_outliers(mt2$mpg, method = "zscore"),
      type = "scree",
      verbose = FALSE
    ),
    variant = "windows"
  )
})


test_that("`plot.see_check_outliers()` multimethods", {
  data(mtcars)
  outliers_list <- performance::check_outliers(
    mtcars,
    method = c(
      "mahalanobis",
      "iqr",
      "zscore"
    )
  )
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers multimethods",
    fig = plot(outliers_list),
    variant = "windows"
  )
})
