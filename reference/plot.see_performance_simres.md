# Plot method for check model for (non-)normality of residuals

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`performance::check_residuals()`](https://easystats.github.io/performance/reference/check_residuals.html)
resp.
[`performance::simulate_residuals()`](https://easystats.github.io/performance/reference/simulate_residuals.html)
function.

## Usage

``` r
# S3 method for class 'see_performance_simres'
plot(
  x,
  linewidth = 0.8,
  size_point = 2,
  size_title = 12,
  size_axis_title = base_size,
  base_size = 10,
  alpha = 0.2,
  alpha_dot = 0.8,
  colors = c("#3aaf85", "#1b6ca8"),
  detrend = FALSE,
  transform = NULL,
  style = theme_lucid,
  ...
)
```

## Arguments

- x:

  An object.

- linewidth:

  Numeric value specifying size of line geoms.

- size_point:

  Numeric specifying size of point-geoms.

- base_size, size_axis_title, size_title:

  Numeric value specifying size of axis and plot titles.

- alpha:

  Numeric value specifying alpha level of the confidence bands.

- alpha_dot:

  Numeric value specifying alpha level of the point geoms.

- colors:

  Character vector of length two, indicating the colors (in hex-format)
  for points and line.

- detrend:

  Logical that decides if Q-Q and P-P plots should be de-trended (also
  known as *worm plots*).

- transform:

  Function to transform the residuals. If `NULL` (default), no
  transformation is applied and uniformly distributed residuals are
  expected. See argument `quantileFuntion` in
  `?DHARMa:::residuals.DHARMa` for more details.

- style:

  A ggplot2-theme.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## See also

See also the vignette about
[[`check_model()`](https://easystats.github.io/performance/reference/check_model.html)](https://easystats.github.io/performance/articles/check_model.html).

## Examples

``` r
data(Salamanders, package = "glmmTMB")
model <- glmmTMB::glmmTMB(
  count ~ mined + spp + (1 | site),
  family = poisson(),
  data = Salamanders
)
simulated_residuals <- performance::simulate_residuals(model)
plot(simulated_residuals)


# or
simulated_residuals <- performance::simulate_residuals(model)
result <- performance::check_residuals(simulated_residuals)
plot(result)
```
