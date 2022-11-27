requiet <- function(package) {
  testthat::skip_if_not_installed(package)
  suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )
}

# load hard deps to use them without namespacing
library(bayestestR)
library(correlation)
library(datawizard)
library(effectsize)
suppressPackageStartupMessages(library(ggplot2))
library(insight)
library(modelbased)
library(parameters)
library(performance)
