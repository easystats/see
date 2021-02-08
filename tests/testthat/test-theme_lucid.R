test_that("`theme_lucid()` works", {
  library(ggplot2)
  library(see)
  expect_s3_class(
    ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
      geom_point(color = "white") +
      theme_lucid(),
    "gg"
  )
})
