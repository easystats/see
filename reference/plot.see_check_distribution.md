# Plot method for classifying the distribution of a model-family

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`performance::check_distribution()`](https://easystats.github.io/performance/reference/check_distribution.html)
function.

## Usage

``` r
# S3 method for class 'see_check_distribution'
plot(x, size_point = 2, panel = TRUE, ...)
```

## Arguments

- x:

  An object.

- size_point:

  Numeric specifying size of point-geoms.

- panel:

  Logical, if `TRUE`, plots are arranged as panels; else, single plots
  are returned.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Examples

``` r
library(performance)
m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
result <- check_distribution(m)
result
#> # Distribution of Model Family
#> 
#> Predicted Distribution of Residuals
#> 
#>  Distribution Probability
#>        normal         47%
#>           chi         16%
#>       tweedie         12%
#> 
#> Predicted Distribution of Response
#> 
#>  Distribution Probability
#>           chi         34%
#>       tweedie         22%
#>         gamma         16%
plot(result)
```
