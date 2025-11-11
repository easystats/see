# Prepare objects for plotting or plot objects

`data_plot()` extracts and transforms an object for plotting, while
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) visualizes
results of functions from different packages in
[easystats-project](https://github.com/easystats). See the documentation
for your object's class:

- [bayestestR::bayesfactor_models()](https://easystats.github.io/see/reference/plot.see_bayesfactor_models.md)

- [bayestestR::bayesfactor_parameters()](https://easystats.github.io/see/reference/plot.see_bayesfactor_parameters.md)

- [bayestestR::equivalence_test()](https://easystats.github.io/see/reference/plot.see_equivalence_test.md)

- [bayestestR::estimate_density()](https://easystats.github.io/see/reference/plot.see_estimate_density.md)

- [bayestestR::hdi()](https://easystats.github.io/see/reference/plot.see_hdi.md)

- [bayestestR::p_direction()](https://easystats.github.io/see/reference/plot.see_p_direction.md)

- [bayestestR::p_significance()](https://easystats.github.io/see/reference/plot.see_p_significance.md)

- [bayestestR::si()](https://easystats.github.io/see/reference/plot.see_si.md)

- [effectsize::effectsize()](https://easystats.github.io/see/reference/plot.see_effectsize_table.md)

- [modelbased::estimate_contrasts()](https://easystats.github.io/see/reference/plot.see_estimate_contrasts.md)

- [parameters::compare_parameters()](https://easystats.github.io/see/reference/plot.see_compare_parameters.md)

- [parameters::describe_distribution()](https://easystats.github.io/see/reference/plot.see_parameters_distribution.md)

- [parameters::model_parameters()](https://easystats.github.io/see/reference/plot.see_parameters_model.md)

- [parameters::principal_components()](https://easystats.github.io/see/reference/plot.see_parameters_pca.md)

- [parameters::n_clusters()](https://easystats.github.io/see/reference/plot.see_n_factors.md)

- [parameters::n_factors()](https://easystats.github.io/see/reference/plot.see_n_factors.md)

- [parameters::simulate_parameters()](https://easystats.github.io/see/reference/plot.see_parameters_simulate.md)

- [performance::check_collinearity()](https://easystats.github.io/see/reference/plot.see_check_collinearity.md)

- [performance::check_heteroscedasticity()](https://easystats.github.io/see/reference/plot.see_check_heteroscedasticity.md)

- [performance::check_homogeneity()](https://easystats.github.io/see/reference/plot.see_check_homogeneity.md)

- [performance::check_normality()](https://easystats.github.io/see/reference/plot.see_check_normality.md)

- [performance::check_outliers()](https://easystats.github.io/see/reference/plot.see_check_outliers.md)

- [performance::compare_performance()](https://easystats.github.io/see/reference/plot.see_compare_performance.md)

- [performance::performance_roc()](https://easystats.github.io/see/reference/plot.see_performance_roc.md)

- [performance::check_predictions()](https://easystats.github.io/see/reference/print.see_performance_pp_check.md)

## Usage

``` r
data_plot(x, ...)

# S3 method for class 'compare_performance'
data_plot(x, data = NULL, ...)
```

## Arguments

- x:

  An object.

- ...:

  Arguments passed to or from other methods.

- data:

  The original data used to create this object. Can be a statistical
  model.

## Details

`data_plot()` is in most situation not needed when the purpose is
plotting, since most
[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-functions in
see internally call `data_plot()` to prepare the data for plotting.  
  
Many [`plot()`](https://rdrr.io/r/graphics/plot.default.html)-functions
have a `data`-argument that is needed when the data or model for
plotting can't be retrieved via `data_plot()`. In such cases,
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) gives an error
and asks for providing data or models.  
  
Most [`plot()`](https://rdrr.io/r/graphics/plot.default.html)-functions
work out-of-the-box, i.e. you don't need to do much more than calling
`plot(<object>)` (see 'Examples'). Some plot-functions allow to specify
arguments to modify the transparency or color of geoms, these are shown
in the 'Usage' section.

## See also

[Package-Vignettes](https://easystats.github.io/see/articles/)

## Examples

``` r
library(bayestestR)
library(rstanarm)

model <<- suppressWarnings(stan_glm(
  Sepal.Length ~ Petal.Width * Species,
  data = iris,
  chains = 2, iter = 200, refresh = 0
))

x <- rope(model, verbose = FALSE)
plot(x)


x <- hdi(model)
plot(x) + theme_modern()


x <- p_direction(model, verbose = FALSE)
plot(x)


model <<- suppressWarnings(stan_glm(
  mpg ~ wt + gear + cyl + disp,
  chains = 2,
  iter = 200,
  refresh = 0,
  data = mtcars
))
x <- equivalence_test(model, verbose = FALSE)
plot(x)
#> Picking joint bandwidth of 0.189
```
