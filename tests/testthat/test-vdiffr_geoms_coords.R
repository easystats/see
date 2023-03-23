test_that("geom and coord functions work correctly", {
  skip_if_not_or_load_if_installed("vdiffr")
  skip_if_not_or_load_if_installed("poorman")

  # coord_radar() ------------------

  data <- iris %>%
    group_by(Species) %>%
    summarise(across(everything(), mean)) %>%
    datawizard::reshape_longer(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "coord_radar() works",
    fig = data %>% ggplot(aes(
      x = name,
      y = value,
      color = Species,
      group = Species
    )) +
      geom_polygon(fill = NA, linewidth = 2) +
      coord_radar(start = -pi / 4)
  )

  # geom_point2() ----------------------

  normal <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
    geom_point(size = 8, alpha = 0.3) +
    theme_modern()
  new <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
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

  # geom_violindot() ----------------------------

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_violindot() works",
    fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_violindot() +
      theme_modern()
  )

  # geom_violinhalf() ---------------------

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_violinhalf() works",
    fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
      geom_violinhalf() +
      theme_modern() +
      scale_fill_material_d()
  )
})
