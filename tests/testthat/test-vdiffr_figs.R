if (getRversion() < "4.1" && require("vdiffr") && require("ggplot2") &&
  require("bayestestR")) {
  test_that("plots are rendered correctly", {
    skip_on_cran()

    # coord_radar() ------------------

    data <- iris %>%
      dplyr::group_by(Species) %>%
      dplyr::summarise_all(mean) %>%
      tidyr::pivot_longer(-Species)

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "coord_radar() works",
      fig = data %>% ggplot(aes(
        x = name, y = value, color = Species,
        group = Species
      )) +
        geom_polygon(fill = NA, size = 2) +
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

    # `geom_poolpoint()` works ----------------

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

    # plot.see_bayesfactor_models() --------------------

    lm0 <- lm(qsec ~ 1, data = mtcars)
    lm1 <- lm(qsec ~ drat, data = mtcars)
    lm2 <- lm(qsec ~ wt, data = mtcars)
    lm3 <- lm(qsec ~ drat + wt, data = mtcars)
    result <- bayesfactor_models(lm1, lm2, lm3, denominator = lm0)

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "``plot.see_bayesfactor_models()` works - 1",
      fig = plot(result, n_pies = "one", value = "probability", sort = TRUE) +
        scale_fill_pizza(reverse = TRUE)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "`plot.see_bayesfactor_models()` works - 2",
      fig = plot(result, n_pies = "many", value = "BF", log = TRUE) +
        scale_fill_pizza(reverse = FALSE)
    )
  })
}
