# Radar coordinate system

Add a radar coordinate system useful for radar charts.

## Usage

``` r
coord_radar(theta = "x", start = 0, direction = 1, ...)
```

## Arguments

- theta:

  variable to map angle to (`x` or `y`)

- start:

  Offset of starting point from 12 o'clock in radians. Offset is applied
  clockwise or anticlockwise depending on value of `direction`.

- direction:

  1, clockwise; -1, anticlockwise

- ...:

  Other arguments to be passed to `ggproto`.

## Examples

``` r
library(ggplot2)

# Create a radar/spider chart with ggplot:
data(iris)
data <- aggregate(iris[-5], list(Species = iris$Species), mean)
data <- datawizard::data_to_long(
  data,
  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)

ggplot(data, aes(x = name, y = value, color = Species, group = Species)) +
  geom_polygon(fill = NA, linewidth = 2) +
  coord_radar(start = -pi / 4)
```
