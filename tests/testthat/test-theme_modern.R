test_that("`theme_modern()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(
    ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
      geom_point() +
      theme_modern(),
    "gg"
  )
})
