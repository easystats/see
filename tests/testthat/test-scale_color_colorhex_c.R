test_that("scale_fill_colorhex works as expected", {
  requiet("vdiffr")
  skip_if(getRversion() < "4.1")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_fill_colorhex_d works",
    fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_violin() +
      scale_fill_colorhex_d(palette = 1014416)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_fill_colorhex_c works",
    fig = ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
      geom_point() +
      scale_color_colorhex_c(palette = 1014416)
  )
})
