test_that("`plot.see_easycormatrix()` works", {
  skip_if_not_or_load_if_installed("correlation")


  result <- correlation(mtcars[, -(8:9)])
  s <- summary(result)
  expect_s3_class(suppressWarnings(plot(s)), "gg")
})
