# Plot method for homogeneity of variances checks

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`performance::check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.html)
function.

## Usage

``` r
# S3 method for class 'see_check_homogeneity'
plot(x, data = NULL, ...)
```

## Arguments

- x:

  An object.

- data:

  The original data used to create this object. Can be a statistical
  model.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Examples

``` r
library(performance)

model <<- lm(len ~ supp + dose, data = ToothGrowth)
result <- check_homogeneity(model)
result
#> OK: There is not clear evidence for different variances across groups (Bartlett Test, p = 0.226).
#> 
plot(result)

```
