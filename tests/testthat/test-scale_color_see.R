test_that("`scale_color_see()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_boxplot() +
      theme_modern() +
      scale_fill_see_d(),
    "gg"
  )

  expect_s3_class(
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
      geom_point() +
      theme_abyss() +
      scale_colour_see(palette = "light"),
    "gg"
  )

  expect_s3_class(
    ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
      geom_point() +
      theme_modern() +
      scale_color_see_c(palette = "rainbow"),
    "gg"
  )
})

test_that("`see_colors()` works", {
  expect_equal(as.vector(see_colors("indigo", "lime")), c("#303960", "#f7fbe1"))
})
