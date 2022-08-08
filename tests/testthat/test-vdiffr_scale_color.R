if (getRversion() >= "4.1" &&
  require("vdiffr") &&
  require("ggplot2")) {
  test_that("scale_color_ functions work correctly", {
    # skip_if_not(.Platform$OS.type == "windows")

    vdiffr::expect_doppelganger(
      title = "scale_color_bluebrown_d() works",
      fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_boxplot() +
        theme_modern() +
        scale_fill_bluebrown_d()
    )

    vdiffr::expect_doppelganger(
      title = "scale_color_social() works",
      fig = ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
        geom_point() +
        theme_modern() +
        scale_color_social_c(palette = "rainbow")
    )

    vdiffr::expect_doppelganger(
      title = "scale_color_pizza_c() works",
      fig = ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
        geom_point() +
        theme_modern() +
        scale_color_pizza_c()
    )

    vdiffr::expect_doppelganger(
      title = "scale_color_pizza_d() works",
      fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_boxplot() +
        theme_modern() +
        scale_fill_pizza_d()
    )

    vdiffr::expect_doppelganger(
      title = "scale_fill_metro_c() works",
      fig = ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
        geom_point() +
        theme_modern() +
        scale_color_metro_c(palette = "rainbow")
    )

    vdiffr::expect_doppelganger(
      title = "scale_fill_metro_d() works",
      fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_violin() +
        theme_modern() +
        scale_fill_metro_d(palette = "ice")
    )

    vdiffr::expect_doppelganger(
      title = "scale_fill_material_c() works",
      fig = ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
        geom_point() +
        theme_modern() +
        scale_color_metro_c(palette = "rainbow")
    )

    vdiffr::expect_doppelganger(
      title = "scale_fill_material_d() works",
      fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_violin() +
        theme_modern() +
        scale_fill_metro_d(palette = "ice")
    )

    vdiffr::expect_doppelganger(
      title = "scale_fill_see_c() works",
      fig = ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
        geom_point() +
        theme_modern() +
        scale_color_see_c(palette = "rainbow")
    )

    vdiffr::expect_doppelganger(
      title = "scale_fill_see_d() works",
      fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_boxplot() +
        theme_modern() +
        scale_fill_see_d()
    )

    vdiffr::expect_doppelganger(
      title = "scale_fill_flat_d() works",
      fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_boxplot() +
        theme_modern() +
        scale_fill_flat_d()
    )

    vdiffr::expect_doppelganger(
      title = "scale_color_flat_c() works",
      fig = ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
        geom_point() +
        theme_modern() +
        scale_color_flat_c(palette = "rainbow")
    )
  })
}
