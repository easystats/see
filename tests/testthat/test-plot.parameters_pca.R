test_that("`plot.see_parameters_pca()` works", {
  library(parameters)
  result <- principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
  expect_s3_class(plot(result), "gg")
})
