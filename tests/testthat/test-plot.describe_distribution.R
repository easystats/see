test_that("`plot.see_parameters_distribution()` works", {
  set.seed(333)
  x <- sample(1:100, 1000, replace = TRUE)
  result <- datawizard::describe_distribution(x)
  expect_s3_class(plot(result), "gg")

  result <- datawizard::describe_distribution(iris)
  expect_true(all(vapply(plot(result), inherits, "gg", FUN.VALUE = logical(1L))))

  result <- datawizard::describe_distribution(iris, select = "Sepal.Length")
  expect_true(all(vapply(plot(result), inherits, "gg", FUN.VALUE = logical(1L))))
})
