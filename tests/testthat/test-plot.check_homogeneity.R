test_that("`plot.see_check_homogeneity()` works", {
  m_lm <<- lm(len ~ supp + dose, data = ToothGrowth)
  result <- check_homogeneity(model)

  expect_s3_class(plot(result), "gg")
})
