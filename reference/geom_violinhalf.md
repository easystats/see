# Half-violin plot

Create a half-violin plot. The half violins are drawn with
[`ggdist::stat_slab()`](https://mjskay.github.io/ggdist/reference/stat_slab.html),
so this function needs the **ggdist** package (version 3.3.3 or later).
By default, each half violin is at most 0.45 times the spacing of the
categorical axis wide (0.45 units on a discrete axis), as before. When
groups are dodged, as with `fill` mapped to a second variable, each
group takes half of its dodge cell, from the cell's centre toward its
side, as in see 0.14.

## Usage

``` r
geom_violinhalf(
  mapping = NULL,
  data = NULL,
  stat = "ydensity",
  position = "dodge",
  trim = TRUE,
  flip = FALSE,
  scale = c("area", "count", "width"),
  show.legend = NA,
  inherit.aes = TRUE,
  interval = FALSE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  Kept for backward compatibility. The density is always computed by
  [`ggdist::stat_slab()`](https://mjskay.github.io/ggdist/reference/stat_slab.html),
  and any value other than `"ydensity"` or
  [ggplot2::StatYdensity](https://ggplot2.tidyverse.org/reference/Stat.html)
  is ignored with a warning.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- trim:

  If `TRUE` (default), trim the tails of the half violins to the range
  of the data. If `FALSE`, extend the tails as
  [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html)
  does.

- flip:

  Should the half-violin plot switch directions? By default, this is
  `FALSE` and each half violin extends to the right of its position (up,
  in a horizontal plot), with its flat side on the left. If
  `flip = TRUE`, all half violins extend to the left (down). Optionally,
  a numeric vector can be supplied indicating which specific geoms
  should be flipped. See examples for more details.

- scale:

  If `"area"` (default), all half violins have the same area (before
  trimming the tails). If `"count"`, areas are scaled proportionally to
  the number of observations. If `"width"`, all half violins have the
  same maximum width.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- interval:

  Logical. If `TRUE`, add the median and the 66% and 95% quantile
  intervals of each group, drawn with
  [`ggdist::stat_slabinterval()`](https://mjskay.github.io/ggdist/reference/stat_slabinterval.html)
  without the slab, as in
  [`ggdist::stat_pointinterval()`](https://mjskay.github.io/ggdist/reference/stat_pointinterval.html).
  Default is `FALSE`.

- ...:

  Other arguments passed to
  [`ggdist::stat_slab()`](https://mjskay.github.io/ggdist/reference/stat_slab.html),
  such as `colour`, `fill`, `alpha`, or `adjust`. As in
  [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html):

  - `bw` sets the bandwidth of the density (default `"nrd0"`): a number,
    or one of `"nrd0"`, `"nrd"`, `"ucv"`, `"bcv"`, `"SJ"`, or `"dpi"`.

  - `kernel` sets the kernel (default `"gaussian"`).

  - `bounds` sets known bounds of the data, which must contain all
    values.

  - `width` (or `height`, in a horizontal plot) is the width of a whole
    violin, so each half violin is half as wide.

  `drop` has no effect. The half violins of groups with fewer than two
  observations, or with all values equal, are not drawn, with a warning,
  but they keep their space when dodged, and their intervals are drawn.
  `.width`, `point_interval`, `show_point`, `show_interval`,
  `fatten_point`, `arrow`, and the `point_` and `interval_` aesthetics,
  such as `point_size`, go to the interval.

## Value

A ggplot2 layer, or a list of two layers if `interval = TRUE`.

## Examples

``` r
library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violinhalf() +
  theme_modern() +
  scale_fill_material_d()


# To flip all half-violin geoms, use `flip = TRUE`:
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violinhalf(flip = TRUE) +
  theme_modern() +
  scale_fill_material_d()


# To flip the half-violin geoms for the first and third groups only
# by passing a numeric vector
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violinhalf(flip = c(1, 3)) +
  theme_modern() +
  scale_fill_material_d()


# To add the median and quantile intervals
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violinhalf(interval = TRUE) +
  theme_modern() +
  scale_fill_material_d()
```
