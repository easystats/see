# Plot method for (non-)constant error variance checks

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`performance::check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.html)
function.

## Usage

``` r
# S3 method for class 'see_check_heteroscedasticity'
plot(
  x,
  data = NULL,
  size_point = 2,
  linewidth = 0.8,
  size_title = 12,
  size_axis_title = base_size,
  base_size = 10,
  ...
)
```

## Arguments

- x:

  An object.

- data:

  The original data used to create this object. Can be a statistical
  model.

- size_point:

  Numeric specifying size of point-geoms.

- linewidth:

  Numeric value specifying size of line geoms.

- base_size, size_axis_title, size_title:

  Numeric value specifying size of axis and plot titles.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## See also

See also the vignette about
[[`check_model()`](https://easystats.github.io/performance/reference/check_model.html)](https://easystats.github.io/performance/articles/check_model.html).

## Examples

``` r
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- performance::check_heteroscedasticity(m)
result
#> Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.042).
#> 
plot(result, data = m) # data required for pkgdown
```
