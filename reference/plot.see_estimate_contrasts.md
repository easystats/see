# Plot method for estimating contrasts

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
the
[`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html)
function.

## Usage

``` r
# S3 method for class 'see_estimate_contrasts'
plot(x, data = NULL, show_data = FALSE, ...)
```

## Arguments

- x:

  An object.

- data:

  The estimated means, obtained with
  [`modelbased::estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.html).

- show_data:

  Logical. If `TRUE`, the plot draws the raw data of the model as
  jittered points behind the lighthouses. The raw data come from
  [`insight::get_data()`](https://easystats.github.io/insight/reference/get_data.html)
  for the model stored in `x`, not from `data`. The plot stops with an
  error in these cases:

  - `show_data` is not a single `TRUE` or `FALSE`.

  - `x` is the output of
    [`data_plot()`](https://easystats.github.io/see/reference/data_plot.md),
    or it does not carry its model, or the model data cannot be
    retrieved.

  - The model transforms the response, for example `log(y)`. This also
    applies with `transform = TRUE`, because the back-transformed
    contrasts are then not differences on the scale of the raw data.

  - The `predict` of the contrasts or of the estimated means is not one
    of `"prediction"`, `"response"`, `"expectation"`, and
    `"invlink(link)"`, for example `predict = "link"`. Means without
    this information, such as a plain data frame, are taken to be on the
    same scale as the contrasts. Other scales, such as contrasts with
    `comparison = "ratio"` or a `transform` function, are not checked.

  - The model data lack the response or a contrasted or `by` variable.

  - The estimated means do not include each value in the data of the
    contrasted and `by` variables, or each combination of their values.
    For example, a numeric `by` variable has values that the means were
    not estimated at.

- ...:

  Arguments passed to or from other methods.

## Value

A ggplot2-object.

## Details

If you estimate the contrasts with `by`, the plot draws each contrast
from the estimated means of its own `by` level. The plot then has one
panel per level, or combination of levels, of the `by` variables. The
estimated means must include the contrasted variables and the `by`
variables, and no other variables that split their rows further. If the
means have no row for the levels of a contrast, the plot does not draw
that contrast and gives a warning.

If you contrast two or more variables jointly, for example with
`contrast = c("am", "cyl")`, the x axis shows their combinations of
levels, such as `"auto, 4"`. The first variable changes slowest along
the axis.

**Interpreting Lighthouse Plots**

Lighthouse plots display both estimated marginal means and the
uncertainty around their pairwise differences (contrasts) in a single
visual:

- **Means and Error Bars:** The points and vertical line ranges display
  the estimated marginal means for each level along with their
  individual confidence intervals.

- **Lighthouse Beams (Projected Uncertainty):** The shaded polygonal
  bands ("beams") represent the pairwise contrasts. A beam originates at
  the point estimate of a reference level and projects toward the
  comparison level. At the target level, the vertical span of the beam
  represents the confidence interval of the estimated difference,
  anchored at the reference mean.

- **Interpretation:** The vertical span of the projected beam at the
  target level corresponds to the confidence interval of the estimated
  difference, evaluated *relative* to the reference point estimate.
  Intuitively, it answers: *"If the reference level is at value Y, where
  do we expect the comparison level to be based on the estimated
  contrast?"*

- **Reading Significance:** Because the beam illustrates the confidence
  interval of the difference, zero difference corresponds to a
  horizontal line from the reference point. If both edges of the beam
  tilt in the same direction (both rising or both falling), the
  confidence interval does not include zero (the difference is
  statistically significant). If one edge rises while the other falls,
  the beam straddles the horizontal line, indicating that a difference
  of zero remains plausible.

\
![Lighthouse plot for two studies: one with significant differences, one
without.](figures/lighthouse.png) *Lighthouse plots of the two studies.
In black, each condition's mean and 95% CI, identical in both panels. In
grey, the beam: the 95% CI of the difference between B and A, cast from
B. The dashed horizontal line, added here as a reading aid, makes it
easier to see which way each edge tilts.*\

## Examples

``` r
# \donttest{
library(modelbased)

model <- lm(Sepal.Width ~ Species, data = iris)
contrasts <- estimate_contrasts(model)
#> We selected `contrast=c("Species")`.
means <- estimate_means(model)
#> We selected `by=c("Species")`.
plot(contrasts, means)


# with the raw data as jittered points
plot(contrasts, means, show_data = TRUE)


# contrasts of `am` within each level of `cyl`, one panel per level
d <- mtcars
d$cyl <- factor(d$cyl)
d$am <- factor(d$am, labels = c("auto", "manual"))
model <- lm(mpg ~ cyl * am, data = d)
contrasts <- estimate_contrasts(model, contrast = "am", by = "cyl")
means <- estimate_means(model, by = c("cyl", "am"))
plot(contrasts, means)


# joint contrasts of `am` and `cyl`, on an axis of combined levels
contrasts <- estimate_contrasts(model, contrast = c("am", "cyl"))
plot(contrasts, means)

# }
```
