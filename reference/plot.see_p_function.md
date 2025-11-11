# Plot method for plotting p-functions (aka consonance functions)

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`parameters::p_function ()`](https://easystats.github.io/parameters/reference/p_function.html).

## Usage

``` r
# S3 method for class 'see_p_function'
plot(
  x,
  colors = c("black", "#1b6ca8"),
  size_point = 1.2,
  linewidth = c(0.7, 0.9),
  size_text = 3,
  alpha_line = 0.15,
  show_labels = TRUE,
  n_columns = NULL,
  show_intercept = FALSE,
  ...
)
```

## Arguments

- x:

  An object returned by
  [`parameters::p_function ()`](https://easystats.github.io/parameters/reference/p_function.html).

- colors:

  Character vector of length two, indicating the colors (in hex-format)
  used when only one parameter is plotted, resp. when panels are plotted
  as facets.

- size_point:

  Numeric specifying size of point-geoms.

- linewidth:

  Numeric value specifying size of line geoms.

- size_text:

  Numeric value specifying size of text labels.

- alpha_line:

  Numeric value specifying alpha of lines indicating the emphasized
  compatibility interval levels (see
  [`?parameters::p_function`](https://easystats.github.io/parameters/reference/p_function.html)).

- show_labels:

  Logical. If `TRUE`, text labels are displayed.

- n_columns:

  For models with multiple components (like fixed and random, count and
  zero-inflated), defines the number of columns for the panel-layout. If
  `NULL`, a single, integrated plot is shown.

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

## Examples

``` r
library(parameters)
#> 
#> Attaching package: ‘parameters’
#> The following object is masked from ‘package:rstanarm’:
#> 
#>     compare_models
model <- lm(Sepal.Length ~ Species + Sepal.Width + Petal.Length, data = iris)
result <- p_function(model)
plot(result, n_columns = 2, show_labels = FALSE)


result <- p_function(model, keep = "Sepal.Width")
plot(result)
```
