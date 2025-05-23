% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.p_significance.R
\name{plot.see_p_significance}
\alias{plot.see_p_significance}
\title{Plot method for practical significance}
\usage{
\method{plot}{see_p_significance}(
  x,
  data = NULL,
  show_intercept = FALSE,
  priors = FALSE,
  alpha_priors = 0.4,
  n_columns = 1,
  ...
)
}
\arguments{
\item{x}{An object.}

\item{data}{The original data used to create this object. Can be a
statistical model.}

\item{show_intercept}{Logical, if \code{TRUE}, the intercept-parameter is included
in the plot. By default, it is hidden because in many cases the
intercept-parameter has a posterior distribution on a very different
location, so density curves of posterior distributions for other parameters
are hardly visible.}

\item{priors}{Logical. If \code{TRUE}, prior distributions are simulated
(using \code{\link[bayestestR:simulate_prior]{bayestestR::simulate_prior()}}) and added
to the plot.}

\item{alpha_priors}{Numeric value specifying alpha for the prior
distributions.}

\item{n_columns}{For models with multiple components (like fixed and random,
count and zero-inflated), defines the number of columns for the
panel-layout. If \code{NULL}, a single, integrated plot is shown.}

\item{...}{Arguments passed to or from other methods.}
}
\value{
A ggplot2-object.
}
\description{
The \code{plot()} method for the \code{bayestestR::p_significance()} function.
}
\examples{
\dontshow{if (identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(rstanarm)
library(bayestestR)
set.seed(123)
m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
result <- p_significance(m)
plot(result)
\dontshow{\}) # examplesIf}
}
