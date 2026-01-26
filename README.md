
# see: Model Visualisation Toolbox for ‘easystats’ and ‘ggplot2’ <img src='man/figures/logo.png' align="right" height="139" />

[![DOI](https://joss.theoj.org/papers/10.21105/joss.03393/status.svg)](https://doi.org/10.21105/joss.03393)
[![downloads](https://cranlogs.r-pkg.org/badges/see)](https://cran.r-project.org/package=see)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/see)](https://cranlogs.r-pkg.org/)

***“Damned are those who believe without seeing”***

*easystats* is a collection of packages that operate in synergy to
provide a consistent and intuitive syntax when working with statistical
models in the R programming language (R Core Team, 2021). Most
*easystats* packages return comprehensive numeric summaries of model
parameters and performance. The *see* package complements these numeric
summaries with a host of functions and tools to produce a range of
publication-ready visualizations for model parameters, predictions, and
performance diagnostics. As a core pillar of *easystats*, the *see*
package helps users to utilize visualization for more informative,
communicable, and well-rounded scientific reporting.

# Installation

[![CRAN](https://www.r-pkg.org/badges/version/see)](https://cran.r-project.org/package=see)
[![see status
badge](https://easystats.r-universe.dev/badges/see)](https://easystats.r-universe.dev)
[![R-CMD-check](https://github.com/easystats/see/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/easystats/see/actions)
[![codecov](https://codecov.io/gh/easystats/see/branch/main/graph/badge.svg)](https://app.codecov.io/gh/easystats/see)

The *see* package is available on CRAN, while its latest development
version is available on R-universe (from *rOpenSci*).

| Type | Source | Command |
|----|----|----|
| Release | CRAN | `install.packages("see")` |
| Development | r-universe | `install.packages("see", repos = "https://easystats.r-universe.dev")` |
| Development | GitHub | `remotes::install_github("easystats/see")` |

Once you have downloaded the package, you can then load it using:

``` r
library("see")
```

> **Tip**
>
> Instead of `library(see)`, use `library(easystats)`. This will make
> all features of the easystats-ecosystem available.
>
> To stay updated, use `easystats::install_latest()`.

# Plotting functions for ‘easystats’ packages

[![Documentation](https://img.shields.io/badge/documentation-see-orange.svg?colorB=E91E63)](https://easystats.github.io/see/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-see-orange.svg?colorB=2196F3)](https://easystats.github.io/see/reference/index.html)

Below we present one or two plotting methods for each *easystats*
package, but many other methods are available. Interested readers are
encouraged to explore the range of examples on the package
[website](https://easystats.github.io/see/articles/).

## [parameters](https://github.com/easystats/parameters)

The *parameters* package converts summaries of regression model objects
into data frames (Lüdecke et al., 2020). The *see* package can take this
transformed object and, for example, create a dot-and-whisker plot for
the extracted regression estimates simply by passing the `parameters`
class object to `plot()`.

``` r
library(parameters)
library(see)

model <- lm(wt ~ am * cyl, data = mtcars)

plot(parameters(model))
```

<img src="man/figures/parameters1-1.png" alt="" width="100%" />

As *see* outputs objects of class `ggplot`, *ggplot2* functions can be
added as layers to the plot the same as with all other *ggplot2*
visualizations. For example, we might add a title using `labs()` from
*ggplot2*.

``` r
library(parameters)
library(see)

model <- lm(wt ~ am * cyl, data = mtcars)

plot(parameters(model)) +
  ggplot2::labs(title = "A Dot-and-Whisker Plot")
```

<img src="man/figures/parameters2-1.png" alt="" width="100%" />

Plotting functions for the **parameters** package are demonstrated [in
this
vignette](https://easystats.github.io/see/articles/parameters.html).

## [bayestestR](https://github.com/easystats/bayestestR)

Similarly, for Bayesian regression model objects, which are handled by
the *bayestestR* package (Makowski et al., 2019), the *see* package
provides special plotting methods relevant for Bayesian models (e.g.,
Highest Density Interval, or *HDI*). Users can fit the model and pass
the model results, extracted via *bayestestR*, to `plot()`.

``` r
library(bayestestR)
library(rstanarm)
library(see)

set.seed(123)
model <- stan_glm(wt ~ mpg, data = mtcars, refresh = 0)
result <- hdi(model, ci = c(0.5, 0.75, 0.89, 0.95))

plot(result)
```

<img src="man/figures/bayestestR-1.png" alt="" width="100%" />

Plotting functions for the **bayestestR** package are demonstrated [in
this
vignette](https://easystats.github.io/see/articles/bayestestR.html).

## [performance](https://github.com/easystats/performance)

The *performance* package is primarily concerned with checking
regression model assumptions (Lüdecke et al., 2021). The *see* package
offers tools to visualize these assumption checks, such as the normality
of residuals. Users simply pass the fit model object to the relevant
*performance* function (`check_normality()` in the example below). Then,
this result can be passed to `plot()` to produce a *ggplot2*
visualization of the check on normality of the residuals.

``` r
library(performance)
library(see)

model <- lm(wt ~ mpg, data = mtcars)
check <- check_normality(model)

plot(check, type = "qq")
```

<img src="man/figures/performance-1.png" alt="" width="100%" />

Plotting functions for the **performance** package are demonstrated [in
this
vignette](https://easystats.github.io/see/articles/performance.html).

## [effectsize](https://github.com/easystats/effectsize)

The *effectsize* package computes a variety of effect size metrics for
fitted models to assesses the practical importance of observed effects
(Ben-Shachar et al., 2020). In conjunction with *see*, users are able to
visualize the magnitude and uncertainty of effect sizes by passing the
model object to the relevant *effectsize* function (`omega_squared()` in
the following example), and then to `plot()`.

``` r
library(effectsize)
library(see)

model <- aov(wt ~ am * cyl, data = mtcars)

plot(omega_squared(model))
```

<img src="man/figures/effectsize-1.png" alt="" width="100%" />

Plotting functions for the **effectsize** package are demonstrated [in
this
vignette](https://easystats.github.io/see/articles/effectsize.html).

## [modelbased](https://github.com/easystats/modelbased)

The *modelbased* package computes model-based estimates and predictions
from fitted models (Makowski et al., 2020a). *see* provides methods to
quickly visualize these model predictions. For the following example to
work, you need to have installed the *emmeans* package first.

``` r
library(modelbased)
library(see)

data(mtcars)
mtcars$gear <- as.factor(mtcars$gear)
model <- lm(mpg ~ wt * gear, data = mtcars)

predicted <- estimate_expectation(model, data = "grid")
plot(predicted, show_data = TRUE)
```

<img src="man/figures/modelbased1-1.png" alt="" width="100%" />

One can also visualize *marginal means* (i.e., the mean at each factor
level averaged over other predictors) using `estimate_means()`, that is
then passed to `plot()`.

``` r
means <- estimate_means(model)

plot(means)
```

<img src="man/figures/modelbased2-1.png" alt="" width="100%" />

Plotting functions for the **modelbased** package are demonstrated [in
this
vignette](https://easystats.github.io/see/articles/modelbased.html).

## [correlation](https://github.com/easystats/correlation)

The *correlation* package provides a unified syntax and human-readable
code to carry out many types of correlation analysis (Makowski et al.,
2020b). A user can run `summary(correlation(data))` to create a
construct a correlation matrix for the variables in a dataframe. With
*see*, this matrix can be passed to `plot()` to visualize these
correlations in a correlation matrix.

``` r
library(correlation)
library(see)

results <- summary(correlation(iris))

plot(results, show_data = "points")
```

<img src="man/figures/correlation-1.png" alt="" width="100%" />

Plotting functions for the **correlation** package are demonstrated [in
this
vignette](https://easystats.github.io/see/articles/correlation.html).

# Themes

### Modern

``` r
library(ggplot2)

ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point2() +
  theme_modern()
```

<img src="man/figures/unnamed-chunk-3-1.png" alt="" width="100%" />

### Lucid

``` r
library(ggplot2)

p <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point2()

p + theme_lucid()
```

<img src="man/figures/unnamed-chunk-4-1.png" alt="" width="100%" />

### Blackboard

``` r
p + theme_blackboard()
```

<img src="man/figures/unnamed-chunk-5-1.png" alt="" width="100%" />

### Abyss

``` r
p + theme_abyss()
```

<img src="man/figures/unnamed-chunk-6-1.png" alt="" width="100%" />

# Palettes

This is just one example of the available palettes. See [this
vignette](https://easystats.github.io/see/articles/seecolorscales.html)
for a detailed overview of palettes and color scales.

### Material design

``` r
p1 <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme_modern(axis.text.angle = 45) +
  scale_fill_material_d()

p2 <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_modern(axis.text.angle = 45) +
  scale_fill_material_d(palette = "ice")

p3 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
  geom_point2() +
  theme_modern() +
  scale_color_material(discrete = FALSE)
```

## Multiple plots

The `plots()` function allows us to plot the figures side by side.

``` r
plots(p1, p2, p3, n_columns = 2)
```

<img src="man/figures/unnamed-chunk-8-1.png" alt="" width="100%" />

The `plots()` function can also be used to add **tags** (*i.e.*, labels
for subfigures).

``` r
plots(p1, p2, p3,
  n_columns = 2,
  tags = paste("Fig. ", 1:3)
)
```

<img src="man/figures/unnamed-chunk-9-1.png" alt="" width="100%" />

# Geoms

## Better looking points

`geom_points2()` and `geom_jitter2()` allow points without borders and
contour.

``` r
normal <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
  geom_point(size = 8, alpha = 0.3) +
  theme_modern()

new <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
  geom_point2(size = 8, alpha = 0.3) +
  theme_modern()

plots(normal, new, n_columns = 2)
```

<img src="man/figures/unnamed-chunk-10-1.png" alt="" width="100%" />

## Half-violin Half-dot plot

Create a half-violin half-dot plot, useful for visualising the
distribution and the sample size at the same time.

``` r
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violindot(fill_dots = "black") +
  theme_modern() +
  scale_fill_material_d()
```

<img src="man/figures/unnamed-chunk-11-1.png" alt="" width="100%" />

## Radar chart (Spider plot)

``` r
library(datawizard)

# prepare the data in tidy format
data <- iris |>
  datawizard::data_group("Species") |>
  datawizard::data_summary(
    Sepal.Length = mean(Sepal.Length),
    Sepal.Width = mean(Sepal.Width),
    Petal.Length = mean(Petal.Length),
    Petal.Width = mean(Petal.Width)
  ) |>
  datawizard::reshape_longer(c(
    "Sepal.Length",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width"
  ))

data |>
  ggplot(aes(
    x = name,
    y = value,
    color = Species,
    group = Species,
    fill = Species
  )) +
  geom_polygon(linewidth = 1, alpha = 0.1) +
  coord_radar() +
  theme_radar()
```

<img src="man/figures/unnamed-chunk-12-1.png" alt="" width="100%" />

# Contributing and Support

In case you want to file an issue or contribute in another way to the
package, please follow [this
guide](https://github.com/easystats/see/blob/master/.github/CONTRIBUTING.md).
For questions about the functionality, you may either contact us via
email or also file an issue.

# Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://easystats.github.io/see/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-Ben-Shachar2020" class="csl-entry">

Ben-Shachar, M. S., Lüdecke, D., & Makowski, D. (2020).
<span class="nocase">e</span>ffectsize: Estimation of effect size
indices and standardized parameters. *Journal of Open Source Software*,
*5*(56), 2815. <https://doi.org/10.21105/joss.02815>

</div>

<div id="ref-Lüdecke2020parameters" class="csl-entry">

Lüdecke, D., Ben-Shachar, M. S., Patil, I., & Makowski, D. (2020).
Extracting, computing and exploring the parameters of statistical models
using R. *Journal of Open Source Software*, *5*(53), 2445.
<https://doi.org/10.21105/joss.02445>

</div>

<div id="ref-Lüdecke2020performance" class="csl-entry">

Lüdecke, D., Ben-Shachar, M. S., Patil, I., Waggoner, P., & Makowski, D.
(2021). <span class="nocase">performance</span>: An R package for
assessment, comparison and testing of statistical models. *Journal of
Open Source Software*, *6*(60), 3139.
<https://doi.org/10.21105/joss.03139>

</div>

<div id="ref-Makowski2019" class="csl-entry">

Makowski, D., Ben-Shachar, M. S., & Lüdecke, D. (2019).
<span class="nocase">bayestestR</span>: Describing effects and their
uncertainty, existence and significance within the Bayesian framework.
*Journal of Open Source Software*, *4*(40), 1541.
<https://doi.org/10.21105/joss.01541>

</div>

<div id="ref-Makowski2020modelbased" class="csl-entry">

Makowski, D., Ben-Shachar, M. S., Patil, I., & Lüdecke, D. (2020a).
Estimation of model-based predictions, contrasts and means. *CRAN*.
<https://github.com/easystats/modelbased>

</div>

<div id="ref-Makowski2020" class="csl-entry">

Makowski, D., Ben-Shachar, M. S., Patil, I., & Lüdecke, D. (2020b).
Methods and algorithms for correlation analysis in R. *Journal of Open
Source Software*, *5*(51), 2306. <https://doi.org/10.21105/joss.02306>

</div>

<div id="ref-base2021" class="csl-entry">

R Core Team. (2021). *R: A language and environment for statistical
computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>

</div>

</div>
