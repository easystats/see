test_that("`coord_radar()` works", {
  if (require("dplyr") && require("tidyr") && require("ggplot2")) {
    data <- iris %>%
      group_by(Species) %>%
      summarise_all(mean) %>%
      pivot_longer(-Species)

    expect_s3_class(
      data %>% ggplot(aes(
        x = name, y = value, color = Species,
        group = Species
      )) +
        geom_polygon(fill = NA, size = 2) +
        coord_radar(start = -pi / 4),
      "gg"
    )
  }
})
