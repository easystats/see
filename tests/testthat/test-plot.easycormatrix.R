test_that("`plot.see_easycormatrix()` works", {
  result <- correlation::correlation(mtcars[, -(8:9)])
  s <- summary(result)
  expect_s3_class(suppressWarnings(plot(s)), "gg")
})
