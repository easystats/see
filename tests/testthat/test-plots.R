test_that("plots() works correctly", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("vdiffr")

  # Basic two-plot combination
  set.seed(123)
  p1 <- ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length, y = Sepal.Width)) +
    ggplot2::geom_point() +
    theme_modern()
  p2 <- ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_density() +
    theme_modern()

  vdiffr::expect_doppelganger(
    title = "plots() basic combination works",
    fig = plots(p1, p2)
  )

  # Three plots with custom layout
  set.seed(123)
  p3 <- ggplot2::ggplot(
    iris,
    ggplot2::aes(x = Species, y = Sepal.Length, fill = Species)
  ) +
    ggplot2::geom_boxplot() +
    theme_modern() +
    scale_fill_see()

  vdiffr::expect_doppelganger(
    title = "plots() with three plots and custom layout works",
    fig = plots(p1, p2, p3, n_columns = 1)
  )

  # With tags
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plots() with tags works",
    fig = plots(p1, p2, n_columns = 2, tags = "A")
  )

  # With custom tags and annotations
  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plots() with custom tags and title works",
    fig = plots(
      p1,
      p2,
      p3,
      n_columns = 1,
      tags = c("Fig. 1", "Fig. 2", "Fig. 3"),
      title = "Multiple Plot Layout",
      subtitle = "Testing plots() function"
    )
  )

  # Different number of rows/columns
  set.seed(123)
  vdiffr::expect_doppelganger(title = "plots() with 2x2 layout works", fig = {
    p4 <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), fill = factor(cyl))) +
      ggplot2::geom_bar() +
      theme_modern() +
      scale_fill_see()

    plots(p1, p2, p3, p4, n_rows = 2, n_columns = 2, tags = "1")
  })
})
