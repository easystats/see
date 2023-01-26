test_that("`plot.see_easycormatrix()` works", {
  skip_if_not_or_load_if_installed("correlation")
  skip_if_not(getRversion() >= "4.1")

  result <- correlation(mtcars[, -c(8:9)])
  s <- summary(result)
  expect_s3_class(plot(s), "gg")
})
