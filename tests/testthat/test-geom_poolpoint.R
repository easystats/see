test_that("`geom_poolpoint()` works - vdiffr", {
  library(ggplot2)
  library(see)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`geom_poolpoint()` works - 1",
    fig = ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
      geom_poolpoint(label = rownames(iris)) +
      scale_color_flat_d() +
      theme_modern()
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`geom_poolpoint()` works - 2",
    fig = ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
      geom_pooljitter(label = rownames(iris)) +
      scale_color_flat_d() +
      theme_modern()
  )
})
