test_that("`plots()` works", {
  skip_if_not_installed("patchwork")

  p1 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
    geom_point()
  p2 <- ggplot(iris, aes(x = Petal.Length)) +
    geom_density()

  expect_s3_class(plots(p1, p2), "ggplot")
})
