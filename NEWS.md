# see 0.1.1

## General

* `geom_point2()` now accepts `size`-aesthetics when mapped to data.
* Themes now get a `base_size` and `base_family` argument, in line with **ggplot** themes, to set the default size and family for plots.

## New themes

* `theme_lucid()`, a light and clear theme.

## New plot-functions

* Added plot-function for `performance::check_model()`.
* Added plot-function for `performance::check_outliers()`.
* Added plot-function for `performance::check_distribution()`.

## New geoms

* `geom_poolpoint()` and  `geom_pooljitter()` to draw pool ball points (points labelled with the observation name).

## Bug fixes

* Fixed issues with color codes in the flat-ui palette.