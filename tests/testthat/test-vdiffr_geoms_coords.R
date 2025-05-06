test_that("geom and coord functions work correctly", {
  skip_if_not_installed("ggplot2")
  # coord_radar() ------------------

  data <- aggregate(iris[1:4], list(Species = iris$Species), mean)
  data <- datawizard::reshape_longer(
    data,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "coord_radar() works",
    fig = ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = name,
        y = value,
        color = Species,
        group = Species
      )
    ) +
      ggplot2::geom_polygon(fill = NA, linewidth = 2) +
      coord_radar(start = -pi / 4)
  )

  # geom_point2() ----------------------

  normal <- ggplot2::ggplot(
    iris,
    ggplot2::aes(x = Petal.Width, y = Sepal.Length)
  ) +
    ggplot2::geom_point(size = 8, alpha = 0.3) +
    theme_modern()
  new <- ggplot2::ggplot(
    iris,
    ggplot2::aes(x = Petal.Width, y = Sepal.Length)
  ) +
    geom_point2(size = 8, alpha = 0.3) +
    theme_modern()

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_point2()` works",
    fig = plots(normal, new, n_columns = 2)
  )

  # geom_poolpoint() ----------------

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`geom_poolpoint()` works - 1",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Petal.Width, y = Sepal.Length, color = Species)
    ) +
      geom_poolpoint(label = rownames(iris)) +
      scale_color_flat_d() +
      theme_modern()
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "`geom_poolpoint()` works - 2",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Petal.Width, y = Sepal.Length, color = Species)
    ) +
      geom_pooljitter(label = rownames(iris)) +
      scale_color_flat_d() +
      theme_modern()
  )

  # geom_violindot() ----------------------------

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_violindot() works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)
    ) +
      geom_violindot() +
      theme_modern()
  )

  # geom_violinhalf() ---------------------

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_violinhalf() works",
    fig = ggplot2::ggplot(
      iris,
      ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)
    ) +
      geom_violinhalf() +
      theme_modern() +
      scale_fill_material_d()
  )
})
