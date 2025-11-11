# Pool ball points

Points labelled with the observation name.

## Usage

``` r
geom_poolpoint(
  label,
  size_text = 3.88,
  size_background = size_text * 2,
  size_point = size_text * 3.5,
  ...
)

geom_pooljitter(
  label,
  size_text = 3.88,
  size_background = size_text * 2,
  size_point = size_text * 3.5,
  jitter = 0.1,
  ...
)
```

## Arguments

- label:

  Label to add inside the points.

- size_text:

  Size of text.

- size_background:

  Size of the white background circle.

- size_point:

  Size of the ball.

- ...:

  Other arguments to be passed to `geom_point`.

- jitter:

  Width and height of position jitter.

## Examples

``` r
library(ggplot2)
library(see)

ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
  geom_poolpoint(label = rownames(iris)) +
  scale_color_flat_d() +
  theme_modern()



ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
  geom_pooljitter(label = rownames(iris)) +
  scale_color_flat_d() +
  theme_modern()
```
