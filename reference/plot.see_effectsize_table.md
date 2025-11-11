# Plot method for effect size tables

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`effectsize::effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.html)
function.

## Usage

``` r
# S3 method for class 'see_effectsize_table'
plot(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Examples

``` r
library(effectsize)
m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
result <- eta_squared(m)
plot(result)
```
