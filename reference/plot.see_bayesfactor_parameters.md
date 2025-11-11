# Plot method for Bayes Factors for a single parameter

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`bayestestR::bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.html)
function.

## Usage

``` r
# S3 method for class 'see_bayesfactor_parameters'
plot(
  x,
  size_point = 2,
  color_rope = "#0171D3",
  alpha_rope = 0.2,
  show_intercept = FALSE,
  ...
)
```

## Arguments

- x:

  An object.

- size_point:

  Numeric specifying size of point-geoms.

- color_rope:

  Character specifying color of ROPE ribbon.

- alpha_rope:

  Numeric specifying transparency level of ROPE ribbon.

- show_intercept:

  Logical, if `TRUE`, the intercept-parameter is included in the plot.
  By default, it is hidden because in many cases the intercept-parameter
  has a posterior distribution on a very different location, so density
  curves of posterior distributions for other parameters are hardly
  visible.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.
