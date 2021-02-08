test_that("`geom_violindot()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
    geom_violindot() +
    theme_modern(), "gg")
})
