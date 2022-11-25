test_that("theme functions work", {
  requiet("vdiffr")
  requiet("poorman")
  skip_if_not(getRversion() >= "4.1")

  library(ggplot2)

  data <- iris %>%
    group_by(Species) %>%
    summarise(across(everything(), mean)) %>%
    datawizard::reshape_longer(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "theme_radar works",
    fig = data %>% ggplot(aes(
      x = name,
      y = value,
      color = Species,
      group = Species,
      fill = Species
    )) +
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
    fig = ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
      geom_point() +
      theme_modern()
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
})
