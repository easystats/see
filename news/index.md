# Changelog

## see 0.13.0

CRAN release: 2026-01-30

### Breaking Changes

- Data points in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for objects
  from
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  were sorted in reverse order when using the `sort` argument. This was
  fixed, thus, `sort = "ascending"` now sorts the data in ascending
  order in plot/math logic.

### Major Changes

- Daniel Lüdecke is now the main maintainer of the package.

- Several [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  methods gain a `theme` argument, to control the visual themes for
  plots. This is in particular useful for plots that consist of several
  single plots, like the one returned by
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html).

### Changes

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  now limits the number of data points for models with many
  observations, to reduce the time for rendering the plot. Use argument
  `maximum_dots` to define the maximum number of data points to show.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  extracts the `show_ci` attribute from objects returned by
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html),
  to show or hide confidence intervals.

### Bug fixes

- Fixed issue in the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
  [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.html)
  with vector input and `group_by` argument.

## see 0.12.0

CRAN release: 2025-09-14

### Major Changes

- The minimum needed R version has been bumped to `4.1`, since
  [correlation](https://easystats.github.io/correlation/), a runtime
  dependency, requires it.

### Changes

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html)
  now also supports objects from
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)
  and
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.html)
  gets a new `"scree"` type option, to create a scree plot of outlier
  statistics.

- Minor re-labelling of axis titles and subtitles in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_residuals()`](https://easystats.github.io/performance/reference/check_residuals.html).

- Updates the package to adapt to the changes in the latest
  [ggplot2](https://ggplot2.tidyverse.org) release.

- Themes get an `axis.text.space` argument, to define the spacing
  between axis lines and axis labels. Due to changes in
  [ggplot2](https://ggplot2.tidyverse.org), the default spacing was
  slightly changed, now it can be set to the desired value using this
  argument.

## see 0.11.0

CRAN release: 2025-03-11

### Changes

- All `theme_*()` function get a `...` argument that is passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- Arguments that change the size of elements in `theme_*()` functions
  (like `plot.title.size` or `axis.text.size` now scale with the
  `base_size` argument, to ensure proper scaling of plots when the
  base-size is changed.

- Added a new theme,
  [`theme_azurelight()`](https://easystats.github.io/see/reference/theme_azurelight.md),
  with a light-blue character, and reduced use of grid lines.

- [`theme_modern()`](https://easystats.github.io/see/reference/theme_modern.md)
  now recognizes a `show.ticks` argument, to add tick marks to the plot
  and slightly increase the distance between axis labels and the related
  axis.

- Color scale functions (those starting with `scale_*()`) get a new
  `"gradient"` palette, which are simply the color values for blue and
  orange colors from that palette. Furthermore, color scale functions
  now automatically select an appropriate palette (usually, `"contrast"`
  or `"gradient"`) depending on whether discrete or continuous color
  scales are requested.

### Bug fixes

- Fixed issue with adding prior layers to plots from
  [`estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.html).

## see 0.10.0

CRAN release: 2025-01-22

### Changes

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`p_function()`](https://easystats.github.io/parameters/reference/p_function.html)
  now checks the values of the `size_length` argument, to give an
  informative error message when the input is not valid.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  now also plots group-levels of random effects (i.e. for mixed models,
  when `model_parameters(x, ..., group_level = TRUE)`).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  gets a `show_direction` argument, to turn off the direction of the
  effect in the plot.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  now gives an informative error message when trying to plot Anova
  tables (which does not work).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.html)
  now better copes with models that have multiple response levels
  (e.g. multinomial models).

- Gains [patchwork](https://patchwork.data-imaginist.com) as a hard
  dependency given its importance for the package.

### Bug fixes

- Fixed issue in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`bayestestR::si()`](https://easystats.github.io/bayestestR/reference/si.html).

- Fixed issue in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  for GAM models.

## see 0.9.0

CRAN release: 2024-09-06

### Changes

- New [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  for
  [`performance::check_dag()`](https://easystats.github.io/performance/reference/check_dag.html).

- Minor improvements to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for methods
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
  and
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.html),
  which also support forthcoming changes in the *parameters* package.

### Bug fixes

- Fixed issue in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  when package *qqplotr* is not installed.

## see 0.8.5

CRAN release: 2024-07-17

### Major Changes

- The minimum needed R version has been bumped to `4.0`.

### Minor Changes

- Improved scaling for detrended QQ plots when package
  [qqplotr](https://github.com/aloy/qqplotr) is not installed. The
  normal and the detrended QQ plots are now visually more similar.

- Gets rid of warnings due to API changes in the
  [datawizard](https://easystats.github.io/datawizard/) package.

### Bug fixes

- Fixed CRAN test failures.

## see 0.8.4

CRAN release: 2024-04-29

### Minor Changes

- Fixes warnings generated from the
  [ggplot2](https://ggplot2.tidyverse.org) 3.5.0 release.

- Small adjustment to size of point geoms for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  plots.

- More arguments to change base font sizes and geom sizes are now passed
  to downstream plot-functions (i.e.,
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  passes arguments to change geom sizes to the underlying
  plot-functions).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.html)
  now supports Bayesian regression models from *brms* and *rstanarm*.

### Bug fixes

- Corrected order of models for `plot.compare_parameters()`.

## see 0.8.3

CRAN release: 2024-03-24

### Major changes

- New [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  for simulated residuals (implemented in the *performance* package).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  was revised and now includes more accurate Q-Q plots for non-Gaussian
  models.

### Minor Changes

- `plot.check_model()` now passes arguments `size_point` and `size_line`
  to the posterior predictive check plot.

- Minor changes regarding the latest update of *ggplot2*.

## see 0.8.2

CRAN release: 2024-02-14

### Minor Changes

- `plot.n_factors()` now shows a dashed line over the bars, indicating
  the cumulate explained variance by the number of factors.

- `plot.check_outliers()` now dodges the x-axis labels, to avoid
  overlapping labels.

## see 0.8.1

CRAN release: 2023-11-03

### Major Changes

- This release changes the licensing model of
  [see](https://easystats.github.io/see/) to an MIT license.

### New features

- There is now a
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
  outputs of
  [`datawizard::data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.html)
  ([\#293](https://github.com/easystats/see/issues/293)).

### Minor Changes

- The [`print()`](https://rdrr.io/r/base/print.html) method for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  now also evaluates the default plot type for posterior predictive
  checks.

- QQ/PP-plots now default to drawing simultaneous testing bands (when
  the `qqplotr` package is available). Previous behavior can be restored
  by setting `method = "pointwise"`.

- Plot method for
  [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html)
  now default to a detrended QQ-plot. Previous behavior can be restored
  by setting `type = "density"`.

- Plot method for
  [`binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.html)
  gains a `show_smooth` argument, to show or hide the smooth line.

- Plot method for
  [`check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.html)
  gains a `x_limits` argument, to limit the x-axis-range. This can be
  useful to “zoom in” certain parts of the plot.

## see 0.8.0

CRAN release: 2023-06-05

### Major Changes

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  no longer produces a normal QQ plot for GLMs. Instead, it now shows a
  half-normal QQ plot of the absolute value of the standardized deviance
  residuals.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  and
  [`performance::check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.html)
  gains a `type` argument, to either create density plots, or discrete
  dots resp. interval plots for posterior predictive checks.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  gains an `n_column` argument, to define the number of columns for the
  diagnostic plots (by default, two columns).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  sometimes failed to create the plot under certain conditions,
  e.g. when the screen or app windows was zoomed-in. If an error occurs,
  a much more informative error message is shown, providing several
  possible solutions to resolve this problem.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  now aligns the labelling with the
  [`print()`](https://rdrr.io/r/base/print.html) method. Hence, the
  legend title is no longer labelled `"Decision on H0"`, but rather
  `"Equivalence"`, to emphasize that we can assume practical equivalence
  for effects, but that we cannot accept the H0 (in a frequentist
  framework).

- Added some examples and cross references between docs. Furthermore, a
  vignette about plotting functions for the *datawizard* package was
  added.

### Bug fixes

- Fixed issue with duplicated legend in the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
  [`performance::check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.html).

- Fixes issue in `plot.binned_residuals()` for models whose residuals
  were completely inside error bounds.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) now works
  when using it on the output of
  [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html)
  with a `select` argument of length 1.

## see 0.7.5

CRAN release: 2023-03-23

### Changes

- Changed the default “yellow” color in
  [`palette_okabeito()`](https://easystats.github.io/see/reference/palette_okabeito.md)
  to `"#F5C710"` instead of `"#F0E442"` to increase visibility against a
  white background. For the original Okabe-Ito palette, set
  `palette = "full_original"` or `palette = "black_first_original"`.

- Deals with deprecated arguments and functions in recent
  [ggplot2](https://ggplot2.tidyverse.org) updates.

## see 0.7.4

CRAN release: 2022-11-25

### Changes

- Updates docs and tests for [ggplot2](https://ggplot2.tidyverse.org)
  release (`3.4.0`).

- New function
  [`scale_color_colorhex()`](https://easystats.github.io/see/reference/scale_color_colorhex.md)
  provides color scales based on palettes from
  <https://www.color-hex.com>
  ([\#245](https://github.com/easystats/see/issues/245)).

- The default for the smoothing bandwidth from
  `plot.check_predictions()` has changed from `"nrd0"` to `"nrd"`, which
  seems to produce better fitting plots for non-Gaussian models.
  Furthermore,
  [`performance::check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.html)
  accepts a `bw` argument (smoothing bandwidth), which is passed down to
  the [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method’s
  density-estimation.

### Bug fixes

- Fixed issues with
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.html)
  for models with convergence issues.

## see 0.7.3

CRAN release: 2022-09-20

### Changes

- Indrajeet Patil is now the maintainer.

- The minimum needed R version has been bumped to `3.6`.

- Replaced deprecated arguments in function calls.

### Bug fixes

- Fixed issues in the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method from
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  for intercept-only models.

## see 0.7.2

CRAN release: 2022-08-10

### Changes

- Added `aesthetics` argument to all `color` and `fill` scale functions
  so that the same scale can be applied to multiple aesthetics and so
  that the functions can be used with other aesthetics from other
  packages (e.g., `edge_color`).

- Added Okabe-Ito color palette (`palette_okabeito`,
  [`scale_color_okabeito()`](https://easystats.github.io/see/reference/scale_color_okabeito.md))
  etc.)

- Several minor improvements.

### Bug fixes

- Fixed CRAN check issues.

## see 0.7.1

CRAN release: 2022-06-20

### Changes

- Changed style for `plot.check_collinearity()`, which is now a
  dot-plot, including error bars.

### Bug fixes

- `plot.check_model()` did not apply the `colors` argument to all
  sub-plots.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  could fail when model objects’ class attribute had more than one
  element.

## see 0.7.0

CRAN release: 2022-03-31

- Improved plotting for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html):

  - using more appropriate plots for binomial models

  - the redundant density-plot of normality-checks was replaced by the
    posterior predictive check plot.

  - includes a plot to investigate overdispersion for count-models.

- Minor improvements for some plot-methods (colour tweaking, labelling,
  …).

## see 0.6.9

CRAN release: 2022-02-15

- Minor changes to catch up with changes from other easystats-packages.

## see 0.6.8

CRAN release: 2021-10-03

### New features

- Add option to show density layers for Bayesian and bootstrapped models
  in [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

- Negative coefficients in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  are now always red.

- Improve support for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  from *emmeans* objects.

- Updated code and examples to work with new API decisions made for the
  *performance* package.

### Bug fixes

- Fix density plots for frequentist models.

## see 0.6.5

### General

- [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  function now uses *patchwork* instead of *gridExtra* to arrange plots
  in a grid.

- Reduced/removed some package dependencies.

- Revise [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`bayestestR::bayesfactor()`](https://easystats.github.io/bayestestR/reference/bayesfactor.html)
  to meet forthcoming changes in the *bayestestR* package.

### New features

- New functions `geom_from_list` and `geoms_from_list` to create geoms
  from lists.

- Plotting for normality check is now supported for `afex_aov` models.

### Bug fixes

- Fixed issue that argument `show_intercept` is no longer ignored in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.html).

## see 0.6.3

CRAN release: 2021-04-09

### New plot functions

- Added plot-function for
  [`parameters::compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.html).

- Added option to `detrend` QQ/PP plots.

### changes

- Plot for SEM models now has arrows pointing from the latent variables
  towards the manifest variables.

- The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  for
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  was revised and should now be more consistent regarding titles and
  subtitles, as well as color schemes and plot order. Furthermore, the
  plot for influential observation was changed in order to better catch
  the potential influential observations.

- The
  [`check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.html)
  plot contains a dashed horizontal line, which makes it to assess the
  homoscedasticity assumption.

- The y-axis label for
  [`check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html)
  plot clarifies that the measure being plotted is VIF. This was unclear
  when this plot was embedded in a grid of plots from
  [`check_model()`](https://easystats.github.io/performance/reference/check_model.html)
  containing multiple checks.

- Plotting methods for
  [`performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.html)
  and
  [`performance_accuracy()`](https://easystats.github.io/performance/reference/performance_accuracy.html)
  show correct labels now.

### Bug fixes

- Fixed issue with wrong labelling of CI-levels for
  [`plot.see_hdi()`](https://easystats.github.io/see/reference/plot.see_hdi.md).

- Fixed minor issues due to breaking changes in other
  easystats-packages.

## see 0.6.2

CRAN release: 2021-02-04

### Changes to functions

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html)
  gains a `alpha`-argument, to change the alpha-level of confidence
  bands.

- The `...` argument for
  [`geom_violindot()`](https://easystats.github.io/see/reference/geom_violindot.md)
  is now also passed to to
  [`geom_violinhalf()`](https://easystats.github.io/see/reference/geom_violinhalf.md),
  allowing, e.g., to change the alpha value of the violin-geoms as well.

- Reorganized order of arguments in
  [`geom_violindot()`](https://easystats.github.io/see/reference/geom_violindot.md),
  so aesthetics arguments like `fill` are not absorbed by partial
  matching for arguments like `fill_dots`.

### Bug fixes

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html)
  shows more accurate QQ-plots.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html)
  show correct confidence bands for QQ- and PP-plots (with `qqplotr`).
  Previous bands were extremely narrow!

## see 0.6.1

CRAN release: 2020-12-06

### Changes to functions

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  now supports models from package *metaBMA*

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  gains a `weight_points` argument, to adjust the point-size depending
  on study-weights for meta-analysis models.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  now labels the x-axis depending on model-type (i.e. “Odds Ratio”
  instead of “Exp(Estimate)”) when this information is available.

### Bug fixes

- Minor fix in [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  for `brmsfit` models.

## see 0.6.0

CRAN release: 2020-09-13

### Breaking changes

- Harmonized spelling of arguments. Formerly, we had `text_size` and
  `size_text`, or just `size` even if only in the context of text
  labels. We now tried to harmonize these kind of arguments to gain more
  consistency throughout the different
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.

### New plot functions

- Added plot-function for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  for brms-meta-analysis models.

- Added plot-function for `performance::pp_check()`.

### Changes to functions

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  gains a `size_text`-argument, to add text values for estimates and
  confidence intervals to the plot (see also vignette).

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  from meta-analysis models (e.g. from *metafor*) gains a `type`
  argument, to create funnel plots with `type = "funnel"`.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`bayestestR::estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.html)
  and
  [`parameters::simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.html)
  were revised, and now also include the point-estimate and error bar.
  Therefore, arguments `centrality` and `ci` were added.

### Bug fixes

- Fixed bug in `plot.model_parameters()` when `standardize` was
  `"basic"`, `"smart"` or `"posthoc"`.

- Fixed wrong axis labelling for ROC-curves (from
  [`performance::performance_roc()`](https://easystats.github.io/performance/reference/performance_roc.html)).

## see 0.5.2

CRAN release: 2020-07-27

### Changes to functions

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  gains a `type` and `size` argument, to plot correlation matrices
  either as tiles or circles.

### Color Scales and Palettes

- Added new color-palettes.

- Palettes for existing color scales have been revamped.

- Added `"light"` palettes for dark themes.

## see 0.5.1

CRAN release: 2020-06-13

### Changes to functions

- Plot-function for
  [`parameters::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html)
  gains `highlight`, `highlight_color` and `size` arguments.

- Plot-function for
  [`parameters::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html)
  now uses thin bars for variables with integer values (instead of less
  precise histogram).

- Plot-function for
  [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html)
  gains `size` and `point_size` arguments.

- Plot-function for
  [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html)
  now also plots normality of random effects, when
  `check_normality(effects = "random")` was called.

- Legend labels nopw show a percentage-sign where applicable (for
  instance, for `plot.hdi()`).

### Bug fixes

- Fixed issue with argument `n_column` in `plot.p_significant()`,
  `plot.hdi()`, and `plot.p_direction()`, which stopped working since R
  4.0.0.

- Fixed issue in `plot.performance_roc()` with swapped x/y axes.

## see 0.5.0

CRAN release: 2020-04-25

### New functions

- [`golden_ratio()`](https://easystats.github.io/see/reference/golden_ratio.md)
  was added as a helper to get nice proportions.

### New plot functions

- Added plot-function for `correlation::summary()`.

- Added plot-function for
  [`parameters::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html).

- Added plot-function for
  [`effectsize::equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html).

- Added plot-function for various effectsize-functions, like
  [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html).

### Changes to functions

- `plot.estimate_density()` now also works for density estimation of
  data frames.

- `plot.equivalence_test()` now also works for frequentist models and
  data frames (see
  [`parameters::equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)).

- Slightly changed plotting-style for `plot.equivalence_test()`.

## see 0.4.1

CRAN release: 2020-03-06

### General

- Reduce package dependencies.

### New plot-functions

- Added plot-function for
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html).

### Changes to functions

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  `model_parameters.rma()` now arranges facets by subgroups (see
  vignettes).

- minor improvements to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::cluster_analysis()`](https://easystats.github.io/parameters/reference/cluster_analysis.html).

- minor improvements to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters.brmsfit()`](https://easystats.github.io/parameters/reference/model_parameters.brmsfit.html).

- Plot-function for
  [`bayestestR::si()`](https://easystats.github.io/bayestestR/reference/si.html)
  gets a `support_only` argument, to plot only the support data or the
  “raw” prior and posterior distributions.

## see 0.4.0

CRAN release: 2020-01-15

### Breaking changes

- Arguments `ncol` and `nrow` in
  [`plots()`](https://easystats.github.io/see/reference/plots.md) were
  renamed into `n_columns` and `n_rows` to be consistent with arguments
  from other functions in the **see**-package.

### New plot-functions

- Added plot-function for
  [`performance::compare_performance()`](https://easystats.github.io/performance/reference/compare_performance.html).

- Added plot-function for
  [`bayestestR::si()`](https://easystats.github.io/bayestestR/reference/si.html).

### Changes to functions

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html)-functions for
  the
  [**bayestestR**-package](https://easystats.github.io/see/articles/bayestestR.html)
  now also support `BFBayesFactor` and `MCMCglmm` objects.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  now uses fixed axis for facets when model is an ordinal or cumulative
  link model (because all facets are on the same scale, and thus
  parameters are easier to compare).

### Bug fixes

- Fixed issue with group coloring for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  when `exponentiate = TRUE`.

- Fixes issue with `plot.point_estimate()` for vectors.

## see 0.3.0

CRAN release: 2019-11-19

### Breaking changes

- `how_to_plot()` was removed for the time being, due to its inadequate
  maintenance effort. It might be re-implemented sometime in the future…

### General

- Improved layout (vertical alignment) for some plotting-functions for
  the **bayestestR**-package, when there was only one parameter. Plots
  now are closer to the x-axis, removing unnecessary empty space.

- Labels and plot annotations are now more “human readable”, and
  labelling is more context sensitive.

### New plot-functions

- Added `coord_radar` for radar charts.

- Added plot-function for
  [`parameters::cluster_analysis()`](https://easystats.github.io/parameters/reference/cluster_analysis.html).

- Added plot-function for
  [`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html).

- Added plot-function for `parameters::parameters_simulate()`.

- Added plot-function for
  [`parameters::n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html).

- Added plot-function for
  [`bayestestR::p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.html).

### Changes to functions

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  gets a `sort`-argument to sort coefficients.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  now also create forest plots for meta-analysis.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`bayestestR::bayesfactor_parameters()`](https://easystats.github.io/bayestestR/reference/bayesfactor_parameters.html)
  only plots facets when necessary.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`performance::check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.html)
  now also plot multiple methods in one plot.

- Following [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  methods get a `n_columns`-argument, so model components like random
  effects or a zero-inflation component can be plotted in a multi-column
  layout:
  [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html),
  [`bayestestR::hdi()`](https://easystats.github.io/bayestestR/reference/hdi.html),
  [`bayestestR::rope()`](https://easystats.github.io/bayestestR/reference/rope.html),
  [`bayestestR::equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html),
  `parameters::model_parameter()`, `parameters::parameters_simulate()`

- Following [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  methods get `priors` and `priors_alpha` arguments, to add a layer with
  prior samples to the plot:
  [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html),
  [`bayestestR::estimate_density()`](https://easystats.github.io/bayestestR/reference/estimate_density.html),
  [`bayestestR::point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.html)

## see 0.2.1

CRAN release: 2019-08-01

### General

- More comprehensive examples available from the
  [package-website](https://easystats.github.io/see/).

- Added new color-palettes.

### New plot-functions

- Added plot-function for
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

- Added plot-function for
  [`bayestestR::point_estimate()`](https://easystats.github.io/bayestestR/reference/point_estimate.html).

### Changes to functions

- The [`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method
  for `bayestestR::bayesfactor_savagedickey()` gets a `rope_color` and a
  `rope_alpha`-argument to plot the “null”-interval, when the
  null-hypothesis is a *region of practical equivalence*, and not just
  the point-null.

- The [`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method
  for
  [`performance::binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.html)
  now also allows to modify size and colors of geoms (related arguments
  are in
  [`performance::binned_residuals()`](https://easystats.github.io/performance/reference/binned_residuals.html)).

### Bug fixes

- Fixed issue with dark themes and text color in facet headings.

## see 0.2.0

CRAN release: 2019-06-19

### General

- [`geom_point2()`](https://easystats.github.io/see/reference/geom_point2.md)
  now accepts `size`-aesthetics when mapped to data.

- Themes now get a `base_size` and `base_family` argument, in line with
  **ggplot** themes, to set the default size and family for plots.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html)-methods now
  work for **bayestestR** functions that work on `emmGrid`-objects
  (created from pairwise comparison with package **emmeans**).

### New themes

- [`theme_lucid()`](https://easystats.github.io/see/reference/theme_lucid.md),
  a light and clear theme.

### New plot-functions

- Added plot-function for
  [`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html).

- Added plot-function for
  [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html).

- Added plot-function for
  [`performance::check_heteroscedasticity()`](https://easystats.github.io/performance/reference/check_heteroscedasticity.html).

- Added plot-function for
  [`performance::check_outliers()`](https://easystats.github.io/performance/reference/check_outliers.html).

- Added plot-function for
  [`performance::check_distribution()`](https://easystats.github.io/performance/reference/check_distribution.html).

- Added plot-function for
  [`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html).

- Added plot-function for
  [`performance::check_homogeneity()`](https://easystats.github.io/performance/reference/check_homogeneity.html).

### New geoms

- [`geom_poolpoint()`](https://easystats.github.io/see/reference/geom_poolpoint.md)
  and
  [`geom_pooljitter()`](https://easystats.github.io/see/reference/geom_poolpoint.md)
  to draw pool ball points (points labelled with the observation name).

### Bug fixes

- Fixed issues with color codes in the flat-ui palette.
