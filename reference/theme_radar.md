# Themes for radar plots

`theme_radar()` is a light, clear theme for ggplot radar-plots, while
`theme_radar_dark()` is a dark variant of `theme_radar()`.

## Usage

``` r
theme_radar(
  base_size = 11,
  base_family = "",
  plot.title.size = 12,
  plot.title.face = "plain",
  plot.title.space = 15,
  plot.title.position = "plot",
  legend.position = "right",
  axis.title.space = 15,
  legend.title.size = 11,
  legend.text.size = 10,
  axis.title.size = 11,
  axis.title.face = "plain",
  axis.text.size = 10,
  axis.text.angle = NULL,
  tags.size = 11,
  tags.face = "plain"
)

theme_radar_dark(
  base_size = 11,
  base_family = "",
  plot.title.size = 12,
  plot.title.face = "plain",
  plot.title.space = 15,
  legend.position = "right",
  axis.title.space = 15,
  legend.title.size = 11,
  legend.text.size = 10,
  axis.title.size = 11,
  axis.title.face = "plain",
  axis.text.size = 10,
  axis.text.angle = NULL,
  tags.size = 11,
  tags.face = "plain"
)
```

## Arguments

- base_size:

  base font size, given in pts.

- base_family:

  base font family

- plot.title.size:

  Title size in pts. Can be "none".

- plot.title.face:

  Title font face ("plain", "italic", "bold", "bold.italic").

- plot.title.space:

  Title spacing.

- plot.title.position:

  Alignment of the plot title/subtitle and caption. The setting for
  plot.title.position applies to both the title and the subtitle. A
  value of "panel" (the default) means that titles and/or caption are
  aligned to the plot panels. A value of "plot" means that titles and/or
  caption are aligned to the entire plot (minus any space for margins
  and plot tag).

- legend.position:

  the default position of legends ("none", "left", "right", "bottom",
  "top", "inside")

- axis.title.space:

  Axis title spacing.

- legend.title.size:

  Legend elements text size in pts.

- legend.text.size:

  Legend elements text size in pts. Can be "none".

- axis.title.size:

  Axis title text size in pts.

- axis.title.face:

  Axis font face ("plain", "italic", "bold", "bold.italic").

- axis.text.size:

  Axis text size in pts.

- axis.text.angle:

  Rotate the x axis labels.

- tags.size:

  Tags text size in pts.

- tags.face:

  Tags font face ("plain", "italic", "bold", "bold.italic").

## See also

[`coord_radar()`](https://easystats.github.io/see/reference/coord_radar.md)

## Examples

``` r
library(ggplot2)

data <- datawizard::reshape_longer(
  aggregate(iris[-5], list(Species = iris$Species), mean),
  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)

ggplot(
  data,
  aes(
    x = name,
    y = value,
    color = Species,
    group = Species,
    fill = Species
  )
) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  coord_radar() +
  theme_radar()
```
