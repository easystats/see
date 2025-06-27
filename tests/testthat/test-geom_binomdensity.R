test_that("geom_binomdensity() works correctly", {
  skip_if_not_installed("ggdist")
  skip_if_not_installed("vdiffr")

  # Basic binary data
  set.seed(123)
  data_binary <- data.frame(
    x = c(rnorm(50, 0, 1), rnorm(50, 2, 1)),
    y = factor(rep(c("Group A", "Group B"), each = 50))
  )

  vdiffr::expect_doppelganger(
    title = "geom_binomdensity() basic works",
    fig = ggplot2::ggplot() +
      geom_binomdensity(
        data_binary,
        x = "x",
        y = "y",
        fill = "steelblue",
        color = NA
      ) +
      theme_modern()
  )

  # Auto scaling (default)
  set.seed(123)
  data_unbalanced <- data.frame(
    x = c(rnorm(30, 0, 1), rnorm(70, 1.5, 1)),
    y = factor(rep(c("Few", "Many"), times = c(30, 70)))
  )

  vdiffr::expect_doppelganger(
    title = "geom_binomdensity() auto scale works",
    fig = ggplot2::ggplot() +
      geom_binomdensity(
        data_unbalanced,
        x = "x",
        y = "y",
        scale = "auto",
        fill = "darkred",
        alpha = 0.7
      ) +
      theme_modern()
  )

  # Proportion scaling
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_binomdensity() proportion scale works",
    fig = ggplot2::ggplot() +
      geom_binomdensity(
        data_unbalanced,
        x = "x",
        y = "y",
        scale = "proportion",
        fill = "darkgreen",
        alpha = 0.7
      ) +
      theme_modern()
  )

  # Density scaling
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_binomdensity() density scale works",
    fig = ggplot2::ggplot() +
      geom_binomdensity(
        data_unbalanced,
        x = "x",
        y = "y",
        scale = "density",
        fill = "purple",
        alpha = 0.7
      ) +
      theme_modern()
  )

  # Custom scale list
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_binomdensity() custom scale works",
    fig = ggplot2::ggplot() +
      geom_binomdensity(
        data_unbalanced,
        x = "x",
        y = "y",
        scale = list(Few = 0.6, Many = 0.4),
        fill = "orange",
        alpha = 0.7
      ) +
      theme_modern()
  )

  # With iris data (binary subset)
  set.seed(123)
  iris_binary <- iris[iris$Species %in% c("setosa", "versicolor"), ]
  iris_binary$Species <- droplevels(iris_binary$Species)

  vdiffr::expect_doppelganger(
    title = "geom_binomdensity() with iris data works",
    fig = ggplot2::ggplot() +
      geom_binomdensity(
        iris_binary,
        x = "Sepal.Length",
        y = "Species",
        fill = "lightblue",
        color = "navy",
        alpha = 0.8
      ) +
      ggplot2::labs(
        title = "Sepal Length Distribution by Species",
        x = "Sepal Length (cm)",
        y = "Species"
      ) +
      theme_modern()
  )
})
