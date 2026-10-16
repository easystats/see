skip_if_not_installed("vdiffr")

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


test_that("`plot.see_check_outliers()` leaves out rows without a distance", {
  co <- performance::check_outliers(stats::lm(mpg ~ disp + drat, data = mtcars))
  dat <- attr(co, "data")
  dist <- dat$Distance_Cook

  # rows the model was not fitted on have no distance, here the two largest
  missing_rows <- order(dist, decreasing = TRUE)[1:2]
  dat$Distance_Cook[missing_rows] <- c(NA, NaN)
  attr(co, "data") <- dat
  attr(co, "influential_obs") <- NULL
  kept <- setdiff(seq_along(dist), missing_rows)
  kept <- kept[order(dist[kept])]

  p <- plot(co, type = "scree", verbose = FALSE)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_identical(p$data$id, kept)
  expect_identical(p$data$mdist, dist[kept])

  p <- plot(co, type = "scree", rescale_distance = TRUE, verbose = FALSE)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_identical(p$data$id, kept)

  # "dots" falls back to the scree plot without leverage values
  p <- plot(co, type = "dots", verbose = FALSE)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_identical(nrow(p$data), length(kept))

  p <- plot(co, type = "count")
  expect_no_warning(suppressMessages(ggplot2::ggplot_build(p)))
  expect_identical(nrow(p$data), length(kept))

  # row labels stay with their rows
  dat$ID <- rownames(mtcars)
  attr(co, "data") <- dat
  p <- plot(co, type = "scree", verbose = FALSE)
  expect_identical(p$data$id, rownames(mtcars)[kept])
})
