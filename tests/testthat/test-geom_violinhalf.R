test_that("`geom_violinhalf()` works", {
  library(ggplot2)
  library(see)

  expect_s3_class(
    ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_violinhalf() +
      theme_modern() +
      scale_fill_material_d(),
    "gg"
  )
})
