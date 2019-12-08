# see 0.3.1

## Breaking changes

* Arguments `ncol` and `nrow` in `plots()` were renamed into `n_columns` and `n_rows` to be consistent with arguments from other functions in the **see**-package.

## New plot-functions

* Added plot-function for `performance::compare_performance()`.

## Changes to functions

* `plot()` for `parameters::model_parameters()` now uses fixed axis for facets when model is an ordinal or cumulative link model (because all facets are on the same scale, and thus parameters are easier to compare).

## Bug fixes

* Fixed issue with group coloring for `parameters::model_parameters()` when `exponentiate = TRUE`.

# see 0.3.0

## Breaking changes

* `how_to_plot()` was removed for the time being, due to its inadequate maintainace effort. It might be re-implemented sometime in the future...

## General

* Improved layout (vertical alignment) for some plotting-functions for the **bayestestR**-package, when there was only one parameter. Plots now are closer to the x-axis, removing unnecessary empty space.
* Labels and plot annotations are now more "human readable", and labelling is more context sensitive.

## New plot-functions

* Added `coord_radar` for radar charts.
* Added plot-function for `parameters::cluster_analysis()`.
* Added plot-function for `parameters::principal_components()`.
* Added plot-function for `parameters::parameters_simulate()`.
* Added plot-function for `parameters::n_factors()`.
* Added plot-function for `bayestestR::p_significance()`.

## Changes to functions

* `plot()` for `parameters::model_parameters()` gets a `sort`-argument to sort coefficients.
* `plot()` for `parameters::model_parameters()` now also create forest plots for meta-analysis.
* `plot()` for `bayestestR::bayesfactor_parameters()` only plots facets when necessary.
* `plot()` for `performance::check_outliers()` now also plot multiple methods in one plot.
* Following `plot()` methods get a `n_columns`-argument, so model components like random effects or a zero-inflation component can be plotted in a multi-column layout: `bayestestR::p_direction()`, `bayestestR::hdi()`, `bayestestR::rope()`, `bayestestR::equivalence_test()`, `parameters::model_parameter()`, `parameters::parameters_simulate()`
* Following `plot()` methods get `priors` and `priors_alpha` arguments, to add a layer with prior samples to the plot: `bayestestR::p_direction()`, `bayestestR::estimate_density()`, `bayestestR::point_estimate()`


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
