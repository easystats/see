# Half-violin Half-dot plot

Create a half-violin half-dot plot, useful for visualising the
distribution and the sample size at the same time. The half violins are
drawn with
[`ggdist::stat_slab()`](https://mjskay.github.io/ggdist/reference/stat_slab.html)
and the dots with
[`ggdist::geom_dots()`](https://mjskay.github.io/ggdist/reference/geom_dots.html),
so this function needs the **ggdist** package (version 3.3.3 or later).
When groups are dodged, as with `fill` mapped to a second variable, each
half violin takes the right half of its dodge cell (the upper half in a
horizontal plot, see
[`geom_violinhalf()`](https://easystats.github.io/see/reference/geom_violinhalf.md)),
and, with the default `position_dots`, its dots go in the other half.
The dots stay in that half when the half violin is flipped, so they
overlap it.

## Usage

``` r
geom_violindot(
  mapping = NULL,
  data = NULL,
  trim = TRUE,
  scale = c("area", "count", "width"),
  show.legend = NA,
  inherit.aes = TRUE,
  dots_size = 0.7,
  dots_color = NULL,
  dots_fill = NULL,
  binwidth = NA,
  position_dots = ggplot2::position_nudge(x = -0.025, y = 0),
  interval = FALSE,
  ...,
  size_dots = dots_size,
  color_dots = dots_color,
  fill_dots = dots_fill
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

- trim:

  If `TRUE` (default), trim the tails of the half violins to the range
  of the data. If `FALSE`, extend the tails as
  [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html)
  does.

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

- binwidth:

  The bin width of the dots. The default, `NA`, lets
  [`ggdist::geom_dots()`](https://mjskay.github.io/ggdist/reference/geom_dots.html)
  choose a bin width that fits the dots into the space beside each half
  violin. A number sets a fixed bin width, in the units of the value
  axis. Dodged groups leave the dots less room, so a fixed bin width can
  make them overflow it, with a warning from ggdist.

- position_dots:

  Position adjustment for dots, either as a string, or the result of a
  call to a position adjustment function. The default nudges the dots by
  `x = -0.025` (`-0.06` with `interval = TRUE`). In a vertical plot,
  this moves them toward smaller x, away from the half violin, and in a
  dodged cell the nudge is at most half the room beside the half violin.
  In a horizontal plot, it moves them along the value axis. The default
  also dodges the dots with their half violins. A position that you set
  is used as given, so the dots are not dodged unless that position
  dodges them.

- interval:

  Logical. If `TRUE`, add the median and the 66% and 95% quantile
  intervals of each group, drawn with
  [`ggdist::stat_slabinterval()`](https://mjskay.github.io/ggdist/reference/stat_slabinterval.html)
  without the slab, as in
  [`ggdist::stat_pointinterval()`](https://mjskay.github.io/ggdist/reference/stat_pointinterval.html).
  Default is `FALSE`.

- ...:

  Other arguments passed to
  [`geom_violinhalf()`](https://easystats.github.io/see/reference/geom_violinhalf.md)
  and to
  [`ggdist::geom_dots()`](https://mjskay.github.io/ggdist/reference/geom_dots.html).
  The arguments of the density, the layout, and the interval (`flip`,
  `stat`, `bw`, `kernel`, `bounds`, `drop`, `adjust`, `n`, `density`,
  `side`, `normalize`, `justification`, `fill_type`, `limits`,
  `p_limits`, `expand`, `breaks`, `outline_bars`, `slab_type`, `.width`,
  `point_interval`, `show_point`, `show_interval`, `fatten_point`,
  `arrow`, and the `point_` and `interval_` aesthetics) go to the half
  violins only. `colour` does not reach the dots when `color_dots` is
  set, and `fill` does not reach them when `fill_dots` is set. As for
  the half violins, a `width` (or `height`) is the width of a whole
  violin, so the dots take half of it. An argument named `position`
  matches `position_dots`. The dots of groups with fewer than two
  observations are still drawn.

- size_dots, dots_size:

  Size adjustment for dots, passed to the `dotsize` argument of
  [`ggdist::geom_dots()`](https://mjskay.github.io/ggdist/reference/geom_dots.html).

- color_dots, dots_color:

  Color adjustment for dots.

- fill_dots, dots_fill:

  Fill adjustment for dots.

## Value

A list of ggplot2 layers.

## Examples

``` r
library(ggplot2)
library(see)

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violindot() +
  theme_modern()


# The dots adapt to the scale of the data
ggplot(iris, aes(x = Species, y = Sepal.Length * 1000, fill = Species)) +
  geom_violindot(interval = TRUE) +
  theme_modern()
```
