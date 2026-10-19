# Plot method for multicollinearity checks

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html)
function.

## Usage

``` r
# S3 method for class 'see_check_collinearity'
plot(
  x,
  data = NULL,
  theme = NULL,
  colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
  size_point = 3.5,
  size_line = 0.8,
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

- theme:

  A ggplot2-theme function, e.g. `theme = theme_lucid()` or
  `theme = ggplot2::theme_dark()`.

- colors:

  Character vector of length two, indicating the colors (in hex-format)
  for points and line.

- size_point:

  Numeric specifying size of point-geoms.

- size_line:

  Numeric value specifying size of line geoms. `linewidth` is an alias
  for `size_line`.

- base_size, size_axis_title, size_title:

  Numeric value specifying size of axis and plot titles.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Details

If a term with a VIF below 2 has an upper confidence limit that is
infinite or above the top of the plot, the plot has a caption. The
caption says that the interval formula fails near VIF = 1, and that this
is not high collinearity.

## Examples

``` r
library(performance)
m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- check_collinearity(m)
result
#> # Check for Multicollinearity
#> 
#> Low Correlation
#> 
#>  Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#>  gear 1.53 [1.04,  2.06]     1.24      0.65     [0.48, 0.96]
#> 
#> Moderate Correlation
#> 
#>  Term  VIF    VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#>    wt 5.05 [2.57,  7.07]     2.25      0.20     [0.14, 0.39]
#>   cyl 5.41 [2.74,  7.58]     2.33      0.18     [0.13, 0.36]
#>  disp 9.97 [4.90, 13.93]     3.16      0.10     [0.07, 0.20]
plot(result)

```
