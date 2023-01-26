test_that("`plot.see_parameters_distribution()` works", {
  set.seed(333)
  x <- sample(1:100, 1000, replace = TRUE)
  result <- describe_distribution(x)
  expect_s3_class(plot(result), "gg")
})
