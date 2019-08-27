# see 0.2.2

## Changes to functions

* `plot()` for `bayestestR::bayesfactor_parameters()` only plots facets when necessary.
* `plot()` for `performance::check_outliers()` now also plot multiple methods in one plot.

# see 0.2.1

## General

* More comprehensive examples available from the [package-website](https://easystats.github.io/see/).
* Added new color-palettes.

## New plot-functions

* Added plot-function for `parameters::model_parameters()`.
* Added plot-function for `bayestestR::point_estimate()`.

## Changes to functions

* The `plot()`-method for `bayestestR::bayesfactor_savagedickey()` gets a `rope_color` and a `rope_alpha`-argument to plot the "null"-interval, when the null-hypothesis is a _region of practical equivalence_, and not just the point-null.
* The `plot()`-method for `performance::binned_residuals()` now also allows to modify size and colors of geoms (related arguments are in `performance::binned_residuals()`).

## Bug fixes

* Fixed issue with dark themes and text color in facet headings.

# see 0.2.0

## General

* `geom_point2()` now accepts `size`-aesthetics when mapped to data.
* Themes now get a `base_size` and `base_family` argument, in line with **ggplot** themes, to set the default size and family for plots.
* `plot()`-methods now work for **bayestestR** functions that work on `emmGrid`-objects (created from pairwise comparison with package **emmeans**).

## New themes

* `theme_lucid()`, a light and clear theme.

## New plot-functions

* Added plot-function for `performance::check_model()`.
* Added plot-function for `performance::check_normality()`.
* Added plot-function for `performance::check_heteroscedasticity()`.
* Added plot-function for `performance::check_outliers()`.
* Added plot-function for `performance::check_distribution()`.
* Added plot-function for `performance::check_collinearity()`.
* Added plot-function for `performance::check_homogeneity()`.

## New geoms

* `geom_poolpoint()` and  `geom_pooljitter()` to draw pool ball points (points labelled with the observation name).

## Bug fixes

* Fixed issues with color codes in the flat-ui palette.