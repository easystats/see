# Plot method for principal component analysis

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html)
function.

## Usage

``` r
# S3 method for class 'see_parameters_pca'
plot(
  x,
  type = c("bar", "line"),
  size_text = 3.5,
  color_text = "black",
  size = 1,
  show_labels = TRUE,
  ...
)
```

## Arguments

- x:

  An object.

- type:

  Character vector, indicating the type of plot. Options are three
  different shapes to represent component loadings; `"bar"` (default)
  for a horizontal bar chart, or `"line"` for a horizontal point and
  line chart.

- size_text:

  Numeric value specifying size of text labels.

- color_text:

  Character specifying color of text labels.

- size:

  Depending on `type`, a numeric value specifying size of bars, lines,
  or segments.

- show_labels:

  Logical. If `TRUE`, text labels are displayed.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Examples

``` r
library(parameters)
data(mtcars)
result <- principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
result
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable |   PC1 |   PC2 |  PC3 |   PC4 |  PC5 |   PC6 | Complexity
#> -------------------------------------------------------------------
#> mpg      | -0.93 |       |      | -0.30 |      |       |       1.30
#> cyl      |  0.96 |       |      |       |      | -0.21 |       1.18
#> disp     |  0.95 |       |      | -0.23 |      |       |       1.16
#> hp       |  0.87 |  0.36 |      |       | 0.30 |       |       1.64
#> drat     | -0.75 |  0.48 | 0.44 |       |      |       |       2.47
#> wt       |  0.88 | -0.35 | 0.26 |       |      |       |       1.54
#> qsec     | -0.54 | -0.81 |      |       |      |       |       1.96
#> 
#> The 6 principal components accounted for 99.30% of the total variance of the original data (PC1 = 72.66%, PC2 = 16.52%, PC3 = 4.93%, PC4 = 2.26%, PC5 = 1.85%, PC6 = 1.08%).
#> 
plot(result)

```
