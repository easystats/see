# Okabe-Ito color palette

The Okabe-Ito color palette was proposed by Okabe and Ito (2008) as a
qualitative color palette that is accessible to people with a variety of
forms of color vision deficiency. In addition to being accessible, it
includes 9 vivid colors that are readily nameable and include colors
that correspond to major primary and secondary colors (e.g., red,
yellow, blue).

## Usage

``` r
scale_color_okabeito(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "color",
  ...
)

scale_fill_okabeito(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "fill",
  ...
)

scale_colour_okabeito(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "color",
  ...
)

scale_colour_oi(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "color",
  ...
)

scale_color_oi(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "color",
  ...
)

scale_fill_oi(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "fill",
  ...
)
```

## Arguments

- palette:

  Character name of palette. Depending on the color scale, can be one of
  `"full"`, `"ice"`, `"rainbow"`, `"complement"`, `"contrast"`,
  `"light"` (for dark themes), `"black_first"`, `full_original`, or
  `black_first_original`. The latter three options are especially for
  the Okabe-Ito color palette. The default is `NULL` and either
  `"contrast"` or `"gradient"` is used (depending on whether `discrete`
  is `TRUE` or `FALSE`), which are the two scale useful for discrete or
  gradient color scales, respectively.

- reverse:

  Boolean indicating whether the palette should be reversed.

- order:

  A vector of numbers from 1 to 9 indicating the order of colors to use
  (default: `1:9`)

- aesthetics:

  A vector of names of the aesthetics that this scale should be applied
  to (e.g., `c('color', 'fill')`).

- ...:

  Additional arguments to pass to
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html).

## Details

The Okabe-Ito palette is included in the base R
[`grDevices::palette.colors()`](https://rdrr.io/r/grDevices/palette.html).
These functions make this palette easier to use with *ggplot2*.

The original Okabe-Ito palette's "yellow" color is `"#F0E442"`. This
color is very bright and often does not show up well on white
backgrounds (see
[here](https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/))
for a discussion of this issue). Accordingly, by default, this function
uses a darker more "amber" color for "yellow" (`"#F5C710"`). This color
is the "yellow" color used in base R \>4.0's [default color
palette](https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/).
The palettes `"full"` and `"black_first"` use this darker yellow color.
For the original yellow color suggested by Okabe and Ito (`"#F0E442"`),
use palettes `"full_original"` or `"black_first_original"`.

The Okabe-Ito palette is only available as a discrete palette. For
color-accessible continuous variables, consider the viridis palettes.

## References

Okabe, M., & Ito, K. (2008). Color universal design (CUD): How to make
figures and presentations that are friendly to colorblind people.
https://jfly.uni-koeln.de/color/#pallet (Original work published 2002)

## Examples

``` r
library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern() +
  scale_fill_okabeito()


ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_oi(palette = "black_first")


# for the original brighter yellow color suggested by Okabe and Ito
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_oi(palette = "full")


ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern() +
  scale_fill_oi(order = c(1, 5, 6, 2, 4, 3, 7))
```
