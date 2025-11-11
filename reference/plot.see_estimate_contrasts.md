# Plot method for estimating contrasts

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html)
function.

## Usage

``` r
# S3 method for class 'see_estimate_contrasts'
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
# \donttest{
library(modelbased)

model <- lm(Sepal.Width ~ Species, data = iris)
contrasts <- estimate_contrasts(model)
#> We selected `contrast=c("Species")`.
means <- estimate_means(model)
#> We selected `by=c("Species")`.
plot(contrasts, means)

# }
```
