test_that("`geom_violinhalf()` works", {
  library(ggplot2)
  library(see)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_violinhalf()` works",
    fig =  ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_violinhalf() +
      theme_modern() +
      scale_fill_material_d()
  )
})
