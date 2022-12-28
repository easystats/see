test_that("`plot.see_equivalence_test()` works", {
  requiet("ggridges")
  m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result <- eta_squared(m)
  expect_s3_class(plot(result), "gg")
})
