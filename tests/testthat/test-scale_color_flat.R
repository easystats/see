test_that("`flat_colors()` works", {
  expect_equal(as.vector(flat_colors("dark red", "teal")), c("#c0392b", "#16a085"))
})

test_that("`scale_color_flat()` works", {
  library(ggplot2)
  library(see)
  expect_s3_class(
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_boxplot() +
      theme_modern() +
      scale_fill_flat_d(),
    "gg"
  )

  expect_s3_class(
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_violin() +
      theme_modern() +
      scale_fill_flat_d(palette = "ice"),
    "gg"
  )

  expect_s3_class(
    ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
      geom_point() +
      theme_modern() +
      scale_color_flat_c(palette = "rainbow"),
    "gg"
  )
})
