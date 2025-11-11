# Plot method for uncertainty or credible intervals

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`bayestestR::hdi()`](https://easystats.github.io/bayestestR/reference/hdi.html)
and related function.

## Usage

``` r
# S3 method for class 'see_hdi'
plot(
  x,
  data = NULL,
  show_intercept = FALSE,
  show_zero = TRUE,
  show_title = TRUE,
  n_columns = 1,
  ...
)
```

## Arguments

- x:

  An object.

- data:

  The original data used to create this object. Can be a statistical
  model.

- show_intercept:

  Logical, if `TRUE`, the intercept-parameter is included in the plot.
  By default, it is hidden because in many cases the intercept-parameter
  has a posterior distribution on a very different location, so density
  curves of posterior distributions for other parameters are hardly
  visible.

- show_zero:

  Logical. If `TRUE`, will add a vertical (dotted) line at 0.

- show_title:

  Logical. If `TRUE`, will show the title of the plot.

- n_columns:

  For models with multiple components (like fixed and random, count and
  zero-inflated), defines the number of columns for the panel-layout. If
  `NULL`, a single, integrated plot is shown.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Examples

``` r
library(rstanarm)
library(bayestestR)
set.seed(123)
m <- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- bayestestR::hdi(m)
result
#> Highest Density Interval 
#> 
#> Parameter                     |       95% HDI
#> ---------------------------------------------
#> (Intercept)                   | [ 4.48, 5.11]
#> Petal.Width                   | [-0.25, 2.10]
#> Speciesversicolor             | [-1.66, 0.20]
#> Speciesvirginica              | [-0.58, 1.47]
#> Petal.Width:Speciesversicolor | [-0.78, 1.81]
#> Petal.Width:Speciesvirginica  | [-1.41, 1.08]
plot(result)
```
