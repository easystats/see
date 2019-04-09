
# see <img src='man/figures/logo.png' align="right" height="139" />

<!-- [![Build -->

<!-- Status](https://travis-ci.org/easystats/see.svg?branch=master)](https://travis-ci.org/easystats/see) -->

<!-- [![codecov](https://codecov.io/gh/easystats/see/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/see) -->

[![Documentation](https://img.shields.io/badge/documentation-see-orange.svg?colorB=E91E63)](https://easystats.github.io/see/)

***“Because you have seen, you have believed”***

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

  - **modern**

<!-- end list -->

``` r
library(ggplot2)

ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) + 
    geom_point() + theme_modern()
```

![](man/figures/unnamed-chunk-4-1.png)<!-- -->
