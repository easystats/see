test_that("scale_color_okabeito() works correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("vdiffr")

  # Discrete fill scale
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_fill_okabeito() default works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)
    ) +
      ggplot2::geom_boxplot() +
      theme_modern() +
      scale_fill_okabeito()
  )

  # Discrete color scale
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_color_okabeito() default works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)
    ) +
      ggplot2::geom_point(size = 3) +
      theme_modern() +
      scale_color_okabeito()
  )

  # Black first palette
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_fill_okabeito() black_first palette works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)
    ) +
      ggplot2::geom_violin() +
      theme_modern() +
      scale_fill_okabeito(palette = "black_first")
  )

  # Full original palette
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_color_okabeito() full_original palette works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)
    ) +
      ggplot2::geom_point(size = 3) +
      theme_modern() +
      scale_color_okabeito(palette = "full_original")
  )

  # Custom order
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_fill_okabeito() custom order works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)
    ) +
      ggplot2::geom_boxplot() +
      theme_modern() +
      scale_fill_okabeito(order = c(2, 4, 6))
  )

  # Reversed palette
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_color_okabeito() reversed works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)
    ) +
      ggplot2::geom_point(size = 3) +
      theme_modern() +
      scale_color_okabeito(reverse = TRUE)
  )

  # Using aliases
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "scale_fill_oi() alias works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)
    ) +
      ggplot2::geom_violin() +
      theme_modern() +
      scale_fill_oi(palette = "black_first")
  )
})
