test_that("`geom_poolpoint()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
    geom_poolpoint(label = rownames(iris)) +
    scale_color_flat_d() +
    theme_modern(), "gg")

  expect_s3_class(ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
    geom_pooljitter(label = rownames(iris)) +
    scale_color_flat_d() +
    theme_modern(), "gg")
})
