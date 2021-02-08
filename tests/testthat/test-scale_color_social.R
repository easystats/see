test_that("`scale_color_social()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
    geom_boxplot() +
    theme_modern() +
    scale_fill_social_d(), "gg")

  expect_s3_class(ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
    geom_violin() +
    theme_modern() +
    scale_fill_social_d(palette = "ice"), "gg")

  expect_s3_class(ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
    geom_point() +
    theme_modern() +
    scale_color_social_c(palette = "rainbow"), "gg")
})

test_that("`social_colors()` works", {
  expect_identical(
    as.vector(social_colors("dark red", "teal")),
    c("#b92b27", "#00b489")
  )
})
