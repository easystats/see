# Plot tabulated data.

Plot tabulated data.

## Usage

``` r
# S3 method for class 'datawizard_tables'
plot(
  x,
  label_values = TRUE,
  show_na = "if_any",
  na_label = "(Missing)",
  error_bar = TRUE,
  ci = 0.95,
  color_fill = "#87CEFA",
  color_error_bar = "#607B8B",
  ...
)

# S3 method for class 'datawizard_table'
plot(
  x,
  label_values = TRUE,
  show_na = "if_any",
  na_label = "(Missing)",
  error_bar = TRUE,
  ci = 0.95,
  color_fill = "#87CEFA",
  color_error_bar = "#607B8B",
  ...
)
```

## Arguments

- x:

  Object created by
  [`datawizard::data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.html).

- label_values:

  Logical. Should values and percentages be displayed at the top of each
  bar.

- show_na:

  Should missing values be dropped? Can be `"if_any"` (default) to show
  the missing category only if any missing values are present,
  `"always"` to always show the missing category, or `"never"` to never
  show the missing category.

- na_label:

  The label given to missing values when they are shown.

- error_bar:

  Logical. Should error bars be displayed? If `TRUE`, confidence
  intervals computed using the Wilson method are shown. See Brown et
  al. (2001) for details.

- ci:

  Confidence Interval (CI) level. Defaults to `0.95` (`95%`).

- color_fill:

  Color to use for category columns (default: `"#87CEFA"`).

- color_error_bar:

  Color to use for error bars (default: `"#607B8B"`).

- ...:

  Unused

## References

Brown, L. D., Cai, T. T., & Dasgupta, A. (2001). Interval estimation for
a binomial proportion. *Statistical Science*, *16*(2), 101-133.
[doi:10.1214/ss/1009213286](https://doi.org/10.1214/ss/1009213286)
