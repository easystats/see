# Plot method for checking model assumptions

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
function. Diagnostic plots for regression models.

## Usage

``` r
# S3 method for class 'see_check_model'
plot(
  x,
  theme = NULL,
  colors = NULL,
  type = c("density", "discrete_dots", "discrete_interval", "discrete_both"),
  n_columns = 2,
  show_smooth = NULL,
  ...
)
```

## Arguments

- x:

  An object.

- theme:

  A ggplot2-theme function, e.g. `theme = theme_lucid()` or
  `theme = ggplot2::theme_dark()`.

- colors:

  Character vector of length two, indicating the colors (in hex-format)
  for points and line.

- type:

  Plot type for the posterior predictive checks plot. Can be `"density"`
  (default), `"discrete_dots"`, `"discrete_interval"` or
  `"discrete_both"` (the `discrete_*` options are appropriate for models
  with discrete - binary, integer or ordinal etc. - outcomes).

- n_columns:

  Number of columns to align plots.

- show_smooth:

  Logical, if `FALSE`, no trend line is drawn in the Linearity,
  Homogeneity of Variance and Influential Observations panels. This
  turns off the LOESS smoother and, for models whose fitted values take
  only a few distinct values, the cell means with their line and error
  bars. If `NULL` (default), the value stored in the
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  object is used, and `TRUE` if there is none. The binned residuals
  panel is not affected.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Details

Larger models (with many observations) may take a longer time to render.
Thus, the number of data points is limited to 2000 by default. Use
`plot(check_model(), maximum_dots = <number>)` (or
`check_model(maximum_dots = <number>)`) to define the number of data
points that should be shown in the plots.

## See also

See also the vignette about
[`check_model()`](https://easystats.github.io/performance/articles/check_model.html).

## Examples

``` r
if (FALSE) { # require("patchwork") && FALSE
library(performance)

model <- lm(qsec ~ drat + wt, data = mtcars)
plot(check_model(model))
}
```
