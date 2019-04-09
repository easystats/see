
# see <img src='man/figures/logo.png' align="right" height="139" />

<!-- [![Build -->

<!-- Status](https://travis-ci.org/easystats/see.svg?branch=master)](https://travis-ci.org/easystats/see) -->

<!-- [![codecov](https://codecov.io/gh/easystats/see/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/see) -->

[![Documentation](https://img.shields.io/badge/documentation-see-orange.svg?colorB=E91E63)](https://easystats.github.io/see/)

***“Damned are those who believe without seeing”***

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/see")
```

``` r
library("see")
```

## Features

### Themes

  - **Modern**

<!-- end list -->

``` r
library(ggplot2)

ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) + 
    geom_point() + theme_modern()
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->

### Palettes

  - **Material design**

<!-- end list -->

``` r
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) + 
    geom_boxplot() + theme_modern() + scale_fill_material_d()
```

![](man/figures/unnamed-chunk-5-1.png)<!-- -->

``` r

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) + 
    geom_violin() + theme_modern() + scale_fill_material_d(palette = "ice")
```

![](man/figures/unnamed-chunk-5-2.png)<!-- -->

``` r

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) + 
    geom_point() + theme_modern() + scale_color_material_c(palette = "rainbow")
```

![](man/figures/unnamed-chunk-5-3.png)<!-- -->
