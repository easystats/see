test_that("`plot.see_easycormatrix()` works", {
  result <- correlation::correlation(mtcars[, -(8:9)])
  s <- summary(result)
  expect_s3_class(suppressWarnings(plot(s)), "gg")
})

test_that("`plot.see_easycormatrix()` snapshot", {
  skip_if_not_installed("vdiffr")

  result <- correlation::correlation(mtcars[, -(8:9)])
  s <- summary(result)
  p <- suppressWarnings(plot(s))
  expect_s3_class(p, c("gg", "ggplot"))

  vdiffr::expect_doppelganger(
    title = "plot.easycormatrix",
    fig = p
  )
})
