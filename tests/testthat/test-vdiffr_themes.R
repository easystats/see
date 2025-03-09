skip_if_not_installed("vdiffr")

test_that("theme functions work", {
  skip_if(getRversion() < "4.1.0")

  data <- iris |>
    datawizard::data_group("Species") |>
    datawizard::data_summary(
      Sepal.Length = mean(Sepal.Length),
      Sepal.Width = mean(Sepal.Width),
      Petal.Length = mean(Petal.Length),
      Petal.Width = mean(Petal.Width)
    ) |>
    datawizard::reshape_longer(c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width"
    ))

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_radar works",
    fig = ggplot(
      data,
      aes(
        x = name,
        y = value,
        color = Species,
        group = Species,
        fill = Species
      )
    ) +
      geom_polygon(
        linewidth = 1,
        alpha = 0.1
      ) +
      coord_radar() +
      theme_radar()
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_modern works",
    fig = ggplot(
      iris,
      aes(x = Sepal.Width, y = Sepal.Length, color = Species)
    ) +
      geom_point() +
      theme_modern()
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_modern with ticks works",
    fig = ggplot(
      iris,
      aes(x = Sepal.Width, y = Sepal.Length, color = Species)
    ) +
      geom_point() +
      theme_modern(show.ticks = TRUE)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_modern works with base_size",
    fig = ggplot(
      iris,
      aes(x = Sepal.Width, y = Sepal.Length, color = Species)
    ) +
      geom_point() +
      theme_modern(base_size = 20)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_lucid works",
    fig = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
      geom_point(color = "white") +
      theme_lucid()
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_lucid works with base_size",
    fig = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
      geom_point(color = "white") +
      theme_lucid(base_size = 20)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_abyss works",
    fig = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
      geom_point(color = "white") +
      theme_abyss()
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_blackboard works",
    fig = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
      geom_point(color = "white") +
      theme_blackboard()
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_blackboard works with base_size",
    fig = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
      geom_point(color = "white") +
      theme_blackboard(base_size = 18)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_azurelight works",
    fig = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
      geom_point() +
      theme_azurelight()
  )
})
