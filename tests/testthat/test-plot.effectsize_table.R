test_that("`plot.see_effectsize_table()` works", {
  library(effectsize)
  m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result <- eta_squared(m)
  expect_s3_class(plot(result), "gg")
})

test_that("`plot.see_equivalence_test()` works", {
  library(effectsize)
  m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
  result <- eta_squared(m)
  expect_s3_class(plot(result), "gg")
})
