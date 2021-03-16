if (getRversion() < "4.1" && require("vdiffr") && require("dplyr") &&
  require("tidyr") && require("ggplot2")) {
  test_that("`coord_radar()` works - vdiffr", {
    skip_on_cran()

    data <- iris %>%
      dplyr::group_by(Species) %>%
      dplyr::summarise_all(mean) %>%
      tidyr::pivot_longer(-Species)

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "coord_radar()` works",
      fig = data %>% ggplot(aes(
        x = name, y = value, color = Species,
        group = Species
      )) +
        geom_polygon(fill = NA, size = 2) +
        coord_radar(start = -pi / 4)
    )
  })
}
