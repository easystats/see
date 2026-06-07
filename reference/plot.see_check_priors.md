# Plot method for prior predictive checks

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`performance::check_priors()`](https://easystats.github.io/performance/reference/check_priors.html)
function.

## Usage

``` r
# S3 method for class 'see_check_priors'
plot(
  x,
  size_point = 2,
  size_boxplot = 0.4,
  size_title = 12,
  size_axis_title = base_size,
  base_size = 10,
  alpha_dot = 0.15,
  alpha_boxplot = 0.35,
  theme = NULL,
  colors = NULL,
  ...
)
```

## Arguments

- x:

  An object.

- size_point:

  Numeric specifying size of point-geoms.

- size_boxplot:

  Numeric value specifying size of boxplot geoms.

- base_size, size_axis_title, size_title:

  Numeric value specifying size of axis and plot titles.

- alpha_dot:

  Numeric value specifying alpha level of the point geoms.

- alpha_boxplot:

  Numeric value specifying alpha of boxplot geoms.

- theme:

  A ggplot2-theme function, e.g. `theme = theme_lucid()` or
  `theme = ggplot2::theme_dark()`.

- colors:

  Character vector of length two, indicating the colors (in hex-format)
  for points and line.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Examples

``` r
# \dontrun{
library(performance)
model <- insight::download_model("stan_prior_checks_1")
plot(performance::check_priors(model, "mmse"))
#> Warning: Logistic regression model has a categorical response variable. You may
#>   need to set `include_response=TRUE` to make it work for predictions.

# }
```
