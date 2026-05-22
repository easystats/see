test_that("borderless geoms work correctly", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("vdiffr")
  data(iris)

  p <- ggplot2::ggplot(
    iris,
    ggplot2::aes(x = Petal.Width, y = Sepal.Length, color = Species)
  ) +
    geom_point_borderless(size = 4) +
    theme_modern()

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_point_borderless_1",
    fig = p
  )

  ggplot2::theme_set(theme_abyss())
  p <- ggplot2::ggplot(
    iris,
    ggplot2::aes(x = Petal.Width, y = Sepal.Length, color = Species)
  ) +
    theme_abyss() +
    geom_point_borderless(size = 4)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_point_borderless_2",
    fig = p
  )

  ggplot2::theme_set(theme_abyss())
  p <- ggplot2::ggplot(
    iris,
    ggplot2::aes(x = Petal.Width, y = Sepal.Length, fill = Species)
  ) +
    theme_abyss() +
    geom_point_halo(size = 12)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_point_halo_1",
    fig = p
  )
})
