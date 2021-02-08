test_that("`bluebrown_colors()` works", {
  expect_equal(as.vector(bluebrown_colors("blue", "brown")), c("#5B93AE", "#61381A"))
})

test_that("`scale_color_bluebrown()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_boxplot() +
      theme_modern() +
      scale_fill_bluebrown_d(),
    "gg"
  )
})
