test_that("`scale_color_pizza()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_boxplot() +
      theme_modern() +
      scale_fill_pizza_d(),
    "gg"
  )

  expect_s3_class(
    ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
      geom_point() +
      theme_modern() +
      scale_color_pizza_c(),
    "gg"
  )
})
