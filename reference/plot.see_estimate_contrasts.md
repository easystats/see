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

## Details

If you estimate the contrasts with `by`, the plot draws each contrast
from the estimated means of its own `by` level. The plot then has one
panel per level, or combination of levels, of the `by` variables. The
estimated means must include the contrasted variables and the `by`
variables, and no other variables that split their rows further. If the
means have no row for the levels of a contrast, the plot does not draw
that contrast and gives a warning.

If you contrast two or more variables jointly, for example with
`contrast = c("am", "cyl")`, the x axis shows their combinations of
levels, such as `"auto, 4"`. The first variable changes slowest along
the axis.

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


# contrasts of `am` within each level of `cyl`, one panel per level
d <- mtcars
d$cyl <- factor(d$cyl)
d$am <- factor(d$am, labels = c("auto", "manual"))
model <- lm(mpg ~ cyl * am, data = d)
contrasts <- estimate_contrasts(model, contrast = "am", by = "cyl")
means <- estimate_means(model, by = c("cyl", "am"))
plot(contrasts, means)


# joint contrasts of `am` and `cyl`, on an axis of combined levels
contrasts <- estimate_contrasts(model, contrast = c("am", "cyl"))
plot(contrasts, means)

# }
```
