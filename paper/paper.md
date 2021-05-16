---
title: "see: R Package for Visualizing Statistical Models"
tags:
  - R
  - easystats
  - ggplot2
  - ggplot2-extension
authors:
- affiliation: 1
  name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206
- affiliation: 2
  name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
- affiliation: 3
  name: Indrajeet Patil
  orcid: 0000-0003-1995-6531
- affiliation: 4
  name: Brenton M. Wiernik
  orcid: 0000-0001-9560-6336
- affiliation: 5
  name: Philip Waggoner
  orcid: 0000-0002-7825-7573
- affiliation: 6
  name: Dominique Makowski
  orcid: 0000-0001-5375-9967
affiliations:
- index: 1
  name:  University Medical Center Hamburg-Eppendorf, Germany
- index: 2
  name: Ben-Gurion University of the Negev, Israel
- index: 3
  name: Center for Humans and Machines, Max Planck Institute for Human Development, Berlin, Germany
- index: 4
  name: Department of Psychology, University of South Florida, USA 
- index: 5
  name: University of Chicago, USA
- index: 6
  name: Nanyang Technological University, Singapore
date: "2021-05-16"
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
link-citations: yes
---



# Summary

The *easystats* is a collection of packages that operate in synergy to provide a consistent and intuitive syntax to work with statistical models in R programming language [@base2021]. Although these packages return comprehensive numeric summaries of model parameters and performance, complementing such summaries with graphical displays can be more informative. The *see* package provides the necessary tools (*ggplot2* geoms, themes, etc.) to visualize the analytic methods supported in this ecosystem.

# Statement of Need

A number of data visualization tools exist in the *ggplot2* framework [@Wickham2016] for graphically exploring statistical models^[For a comprehensive collection of available packages, see: <https://exts.ggplot2.tidyverse.org/gallery/>]. A few of them provide ready-made plots or geometric layers for conveniently preparing common visualizations, but without linking them to particular statistical analyses (e.g., *ggrepel*, *ggalluvial*, *ggridges*, *ggdist*, etc.). A few other packages *do* provide visualizations linked with statistical analyses (e.g., *ggpubr*, *tidymv*, *ggstatsplot*, *survminer*, etc.), but they tend to be more specialized and focus only certain kind of statistical analysis. For example, the *ggstatsplot* package [@Patil2021] offers visualizations only for statistical analysis of one-way factorial designs. 

The *see* package, on the other hand, is designed to work with statistical models and provides a much more comprehensive visualization toolbox that covers all stages of statistical analysis and reporting. Additionally, given that it is part of a larger *easystats*-universe, it also embodies a common syntax and philosophy of this ecosystem.

# Features

For the sake of brevity, we will discuss only one plotting method for each package. Several other methods are available and the readers are encouraged to see more examples of the package website: <https://easystats.github.io/see/>

## Visualizing parameters of a model

The *parameters* package [@Lüdecke2020parameters] helps convert regression model objects into dataframes. The *see* package can then create dot-and-whisker plots for regression estimates in these dataframes:


```r
library(parameters)
library(see)
mod <- lm(wt ~ am * cyl, mtcars)
plot(parameters(mod))
```


\includegraphics[width=1\linewidth]{paper_files/figure-latex/unnamed-chunk-2-1} 

In case of a Bayesian regression model, which are handled by the *bayestestR* package [@Makowski2019], the *see* package also provides special plotting methods relevant only for Bayesian models (e.g., Highest Density Interval (HDI)).


```r
library(bayestestR)
library(rstanarm)
mod <- stan_glm(wt ~ mpg, data = mtcars, refresh = 0)
result <- hdi(mod, ci = c(0.5, 0.75, 0.89, 0.95))
plot(result)
```


\includegraphics[width=1\linewidth]{paper_files/figure-latex/unnamed-chunk-3-1} 

## Visualizing model fit 

The *performance* package [@Lüdecke2020performance] provides functions to check assumptions behind regression models and to compute performance measures for model fits. The *see* package can help visualize these assumptions checks (e.g., normality of residuals):


```r
library(performance)
mod <- lm(wt ~ mpg, mtcars)
plot(check_normality(mod))
#> Warning: Non-normality of residuals detected (p = 0.016).
```


\includegraphics[width=1\linewidth]{paper_files/figure-latex/unnamed-chunk-4-1} 

## Visualizing effect sizes

In addition to providing tabular summaries of regression model objects, the *easystats* ecosystem also provides the *effectsize* package [@Ben-Shachar2020] to assess practical importance of these effects. The *see* package can also be used to visualize the magnitude and uncertainty of these effect sizes:


```r
library(effectsize)
mod <- aov(wt ~ am * cyl, mtcars)
plot(omega_squared(mod))
```


\includegraphics[width=1\linewidth]{paper_files/figure-latex/unnamed-chunk-5-1} 

## Visualizing marginal effects

The *modelbased* package [@Makowski2020modelbased] can be used to compute marginal means for regression models and the *see* package provides functions to visualize these marginal effects.


```r
library(modelbased)

mod <- stan_glm(wt ~ as.factor(cyl), data = mtcars, refresh = 0)
contrasts <- estimate_contrasts(mod)
means <- estimate_means(mod)

plot(contrasts, means)
```


\includegraphics[width=1\linewidth]{paper_files/figure-latex/unnamed-chunk-6-1} 

## Visualizing correlation matrices

The *correlation* package [@Makowski2020] provides a unified syntax to carry out various kinds of correlation analyses, and the *see* package can easily visualize these correlations in a matrix.


```r
library(correlation)
result <- correlation(iris, type = "percentage")
plot(summary(result))
```


\includegraphics[width=1\linewidth]{paper_files/figure-latex/unnamed-chunk-7-1} 

# Licensing and Availability

*see* is licensed under the GNU General Public License (v3.0), with all source code stored at GitHub (<https://github.com/easystats/see>), and with a corresponding issue tracker for bug reporting and feature enhancements. In the spirit of honest and open science, we encourage requests, tips for fixes, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.

# Acknowledgments

*see* is part of the collaborative [*easystats*](https://github.com/easystats/easystats) ecosystem. Thus, we thank the [members of easystats](https://github.com/orgs/easystats/people) as well as the users.

# References


