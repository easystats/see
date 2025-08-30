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

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works default method",
    fig = plot(performance::check_outliers(model, verbose = FALSE))
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works bars method",
    fig = plot(performance::check_outliers(model, type = "bars", verbose = FALSE))
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works scree method",
    fig = plot(performance::check_outliers(model, type = "scree", verbose = FALSE))
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plot.see_check_outliers works scree method, z-score",
    fig = plot(performance::check_outliers(mt2$mpg, method = "zscore"), type = "scree")
  )
})
