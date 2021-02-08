test_that("`theme_radar()` works", {
  if (require("ggplot2") && require("dplyr") && require("tidyr")) {
    data <- iris %>%
      group_by(Species) %>%
      summarise_all(mean) %>%
      pivot_longer(-Species)

    expect_s3_class(
      data %>% ggplot(aes(
        x = name, y = value, color = Species,
        group = Species, fill = Species
      )) +
        geom_polygon(
          size = 1,
          alpha = 0.1
        ) +
        coord_radar() +
        theme_radar(),
      "gg"
    )
  }
})
