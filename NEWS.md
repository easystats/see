# see 0.7.3

## Changes

* Indrajeet Patil is now the maintainer.

* Replaced deprecated arguments in function calls.

## Bug fixes

* Fixed issues in the `plot()` method from `model_parameters()` for
  intercept-only models.

# see 0.7.2

## Changes

* Added `aesthetics` argument to all `color` and `fill` scale functions so that
  the same scale can be applied to multiple aesthetics and so that the functions
  can be used with other aesthetics from other packages (e.g., `edge_color`).
  
* Added Okabe-Ito color palette (`palette_okabeito`, `scale_color_okabeito()`) etc.)

* Several minor improvements.

## Bug fixes

* Fixed CRAN check issues.

# see 0.7.1

## Changes

* Changed style for `plot.check_collinearity()`, which is now a dot-plot, 
  including error bars.

## Bug fixes

* `plot.check_model()` did not apply the `colors` argument to all sub-plots.

* `plot()` for `model_parameters()` could fail when model objects' class
  attribute had more than one element.

# see 0.7.0

* Improved plotting for `performance::check_model()`:

  - using more appropriate plots for binomial models

  - the redundant density-plot of normality-checks was replaced by the
    posterior predictive check plot.
    
  - includes a plot to investigate overdispersion for count-models.

* Minor improvements for some plot-methods (colour tweaking, labelling, ...).

# see 0.6.9

* Minor changes to catch up with changes from other easystats-packages.

# see 0.6.8

## New features

* Add option to show density layers for Bayesian and bootstrapped models in
  `plot()` for `parameters::model_parameters()`.

* Negative coefficients in `plot()` for `parameters::model_parameters()` are
  now always red.
  
* Improve support for `parameters::model_parameters()` from *emmeans* objects.

* Updated code and examples to work with new API decisions made for the
  *performance* package.

## Bug fixes

* Fix density plots for frequentist models.

# see 0.6.5

## General

* `check_model()` function now uses *patchwork* instead of *gridExtra* to
  arrange plots in a grid.

* Reduced/removed some package dependencies.

* Revise `plot()` for `bayestestR::bayesfactor()` to meet forthcoming changes in
  the _bayestestR_ package.

## New features

* New functions `geom_from_list` and `geoms_from_list` to create geoms from
  lists.

* Plotting for normality check is now supported for `afex_aov` models.

## Bug fixes

* Fixed issue that argument `show_intercept` is no longer ignored in `plot()`
  for `rope()`.

# see 0.6.3

## New plot functions

* Added plot-function for `parameters::compare_parameters()`.

* Added option to `detrend` QQ/PP plots.

## changes

* Plot for SEM models now has arrows pointing from the latent variables towards
  the manifest variables.

* The `plot()` method for `check_model()` was revised and should now be more
  consistent regarding titles and subtitles, as well as color schemes and plot
  order. Furthermore, the plot for influential observation was changed in order
  to better catch the potential influential observations.

* The `check_heteroscedasticity()` plot contains a dashed horizontal line, which
  makes it to assess the homoscedasticity assumption.

* The y-axis label for `check_collinearity()` plot clarifies that the measure
  being plotted is VIF. This was unclear when this plot was embedded in a grid
  of plots from `check_model()` containing multiple checks.

* Plotting methods for `performance_roc()` and `performance_accuracy()` show
  correct labels now.

## Bug fixes

* Fixed issue with wrong labelling of CI-levels for `plot.see_hdi()`.

* Fixed minor issues due to breaking changes in other easystats-packages.

# see 0.6.2

## Changes to functions

* `plot()` for `performance::check_normality()` gains a `alpha`-argument, to
  change the alpha-level of confidence bands.

* The `...` argument for `geom_violindot()` is now also passed to to
  `geom_violinhalf()`, allowing, e.g., to change the alpha value of the
  violin-geoms as well.

* Reorganized order of arguments in `geom_violindot()`, so aesthetics arguments
  like `fill` are not absorbed by partial matching for arguments like
  `fill_dots`.

## Bug fixes

* `plot()` for `performance::check_normality()` shows more accurate QQ-plots.

* `plot()` for `performance::check_normality()` show correct confidence bands
  for QQ- and PP-plots (with `qqplotr`). Previous bands were extremely narrow!

# see 0.6.1

## Changes to functions

* `plot()` for `parameters::model_parameters()` now supports models from package
  *metaBMA*

* `plot()` for `parameters::model_parameters()` gains a `weight_points`
  argument, to adjust the point-size depending on study-weights for
  meta-analysis models.

* `plot()` for `parameters::model_parameters()` now labels the x-axis depending
  on model-type (i.e. "Odds Ratio" instead of "Exp(Estimate)") when this
  information is available.

## Bug fixes

* Minor fix in `plot()` for `parameters::model_parameters()` for `brmsfit`
  models.

# see 0.6.0

## Breaking changes

* Harmonized spelling of arguments. Formerly, we had `text_size` and
  `size_text`, or just `size` even if only in the context of text labels. We now
  tried to harmonize these kind of arguments to gain more consistency throughout
  the different `plot()` methods.

## New plot functions

* Added plot-function for `parameters::model_parameters()` for
  brms-meta-analysis models.

* Added plot-function for `performance::pp_check()`.

## Changes to functions

* `plot()` for `parameters::model_parameters()` gains a `size_text`-argument, to
  add text values for estimates and confidence intervals to the plot (see also
  vignette).

* `plot()` for `parameters::model_parameters()` from meta-analysis models (e.g.
  from *metafor*) gains a `type` argument, to create funnel plots with `type =
  "funnel"`.

* `plot()` for `bayestestR::estimate_density()` and
  `parameters::simulate_parameters()` were revised, and now also include the
  point-estimate and error bar. Therefore, arguments `centrality` and `ci` were
  added.

## Bug fixes

* Fixed bug in `plot.model_parameters()` when `standardize` was `"basic"`,
  `"smart"` or `"posthoc"`.

* Fixed wrong axis labelling for ROC-curves (from
  `performance::performance_roc()`).

# see 0.5.2

## Changes to functions

* `plot()` for `correlation::correlation()` gains a `type` and `size` argument,
  to plot correlation matrices either as tiles or circles.

## Color Scales and Palettes

* Added new color-palettes.

* Palettes for existing color scales have been revamped.

* Added `"light"` palettes for dark themes.

# see 0.5.1

## Changes to functions

* Plot-function for `parameters::describe_distribution()` gains `highlight`,
  `highlight_color` and `size` arguments.

* Plot-function for `parameters::describe_distribution()` now uses thin bars for
  variables with integer values (instead of less precise histogram).

* Plot-function for `performance::check_normality()` gains `size` and
  `point_size` arguments.

* Plot-function for `performance::check_normality()` now also plots normality of
  random effects, when `check_normality(effects = "random")` was called.

* Legend labels nopw show a percentage-sign where applicable (for instance, for
  `plot.hdi()`).

## Bug fixes

* Fixed issue with argument `n_column` in `plot.p_significant()`, `plot.hdi()`,
  and `plot.p_direction()`, which stopped working since R 4.0.0.

* Fixed issue in `plot.performance_roc()` with swapped x/y axes.

# see 0.5.0

## New functions

* `golden_ratio()` was added as a helper to get nice proportions.

## New plot functions

* Added plot-function for `correlation::summary()`.

* Added plot-function for `parameters::describe_distribution()`.

* Added plot-function for `effectsize::equivalence_test()`.

* Added plot-function for various effectsize-functions, like
  `effectsize::eta_squared()`.

## Changes to functions

* `plot.estimate_density()` now also works for density estimation of data
  frames.

* `plot.equivalence_test()` now also works for frequentist models and data
  frames (see `parameters::equivalence_test()`).

* Slightly changed plotting-style for `plot.equivalence_test()`.

# see 0.4.1

## General

* Reduce package dependencies.

## New plot-functions

* Added plot-function for `correlation::correlation()`.

## Changes to functions

* `plot()` for `model_parameters.rma()` now arranges facets by subgroups (see
  vignettes).

* minor improvements to `plot()` for `parameters::cluster_analysis()`.

* minor improvements to `plot()` for `parameters::model_parameters.brmsfit()`.

* Plot-function for `bayestestR::si()` gets a `support_only` argument, to plot
  only the support data or the "raw" prior and posterior distributions.

# see 0.4.0

## Breaking changes

* Arguments `ncol` and `nrow` in `plots()` were renamed into `n_columns` and
  `n_rows` to be consistent with arguments from other functions in the
  **see**-package.

## New plot-functions

* Added plot-function for `performance::compare_performance()`.

* Added plot-function for `bayestestR::si()`.

## Changes to functions

* `plot()`-functions for the
  [**bayestestR**-package](https://easystats.github.io/see/articles/bayestestR.html)
  now also support `BFBayesFactor` and `MCMCglmm` objects.

* `plot()` for `parameters::model_parameters()` now uses fixed axis for facets
  when model is an ordinal or cumulative link model (because all facets are on
  the same scale, and thus parameters are easier to compare).

## Bug fixes

* Fixed issue with group coloring for `parameters::model_parameters()` when
  `exponentiate = TRUE`.

* Fixes issue with `plot.point_estimate()` for vectors.

# see 0.3.0

## Breaking changes

* `how_to_plot()` was removed for the time being, due to its inadequate
  maintenance effort. It might be re-implemented sometime in the future...

## General

* Improved layout (vertical alignment) for some plotting-functions for the
  **bayestestR**-package, when there was only one parameter. Plots now are
  closer to the x-axis, removing unnecessary empty space.

* Labels and plot annotations are now more "human readable", and labelling is
  more context sensitive.

## New plot-functions

* Added `coord_radar` for radar charts.

* Added plot-function for `parameters::cluster_analysis()`.

* Added plot-function for `parameters::principal_components()`.

* Added plot-function for `parameters::parameters_simulate()`.

* Added plot-function for `parameters::n_factors()`.

* Added plot-function for `bayestestR::p_significance()`.

## Changes to functions

* `plot()` for `parameters::model_parameters()` gets a `sort`-argument to sort
  coefficients.

* `plot()` for `parameters::model_parameters()` now also create forest plots for
  meta-analysis.

* `plot()` for `bayestestR::bayesfactor_parameters()` only plots facets when
  necessary.

* `plot()` for `performance::check_outliers()` now also plot multiple methods in
  one plot.

* Following `plot()` methods get a `n_columns`-argument, so model components
  like random effects or a zero-inflation component can be plotted in a
  multi-column layout: `bayestestR::p_direction()`, `bayestestR::hdi()`,
  `bayestestR::rope()`, `bayestestR::equivalence_test()`,
  `parameters::model_parameter()`, `parameters::parameters_simulate()`

* Following `plot()` methods get `priors` and `priors_alpha` arguments, to add a
  layer with prior samples to the plot: `bayestestR::p_direction()`,
  `bayestestR::estimate_density()`, `bayestestR::point_estimate()`

# see 0.2.1

## General

* More comprehensive examples available from the
  [package-website](https://easystats.github.io/see/).

* Added new color-palettes.

## New plot-functions

* Added plot-function for `parameters::model_parameters()`.

* Added plot-function for `bayestestR::point_estimate()`.

## Changes to functions

* The `plot()`-method for `bayestestR::bayesfactor_savagedickey()` gets a
  `rope_color` and a `rope_alpha`-argument to plot the "null"-interval, when the
  null-hypothesis is a _region of practical equivalence_, and not just the
  point-null.

* The `plot()`-method for `performance::binned_residuals()` now also allows to
  modify size and colors of geoms (related arguments are in
  `performance::binned_residuals()`).

## Bug fixes

* Fixed issue with dark themes and text color in facet headings.

# see 0.2.0

## General

* `geom_point2()` now accepts `size`-aesthetics when mapped to data.

* Themes now get a `base_size` and `base_family` argument, in line with
  **ggplot** themes, to set the default size and family for plots.

* `plot()`-methods now work for **bayestestR** functions that work on
  `emmGrid`-objects (created from pairwise comparison with package
  **emmeans**).

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

* `geom_poolpoint()` and `geom_pooljitter()` to draw pool ball points (points
  labelled with the observation name).

## Bug fixes

* Fixed issues with color codes in the flat-ui palette.

