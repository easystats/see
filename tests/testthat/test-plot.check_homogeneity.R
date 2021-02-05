test_that("`plot.see_check_homogeneity()` works", {
  library(performance)
  model <<- lm(len ~ supp + dose, data = ToothGrowth)
  result <- check_homogeneity(model)

  expect_s3_class(plot(result), "gg")
})
