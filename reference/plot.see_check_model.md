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
  style = theme_lucid,
  colors = NULL,
  type = c("density", "discrete_dots", "discrete_interval", "discrete_both"),
  n_columns = 2,
  ...
)
```

## Arguments

- x:

  An object.

- style:

  A ggplot2-theme.

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
[[`check_model()`](https://easystats.github.io/performance/reference/check_model.html)](https://easystats.github.io/performance/articles/check_model.html).

## Examples

``` r
library(performance)

model <- lm(qsec ~ drat + wt, data = mtcars)
plot(check_model(model))
#> Ignoring unknown labels:
#> • size : ""
```
