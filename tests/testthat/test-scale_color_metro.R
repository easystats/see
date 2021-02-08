test_that("`metro_colors()` works", {
  expect_equal(as.vector(metro_colors("dark red", "teal")), c("#a20025", "#00aba9"))
})

test_that("`scale_color_metro()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_boxplot() +
      theme_modern() +
      scale_fill_metro_d(),
    "gg"
  )

  expect_s3_class(
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_violin() +
      theme_modern() +
      scale_fill_metro_d(palette = "ice"),
    "gg"
  )

  expect_s3_class(
    ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
      geom_point() +
      theme_modern() +
      scale_color_metro_c(palette = "rainbow"),
    "gg"
  )
})
