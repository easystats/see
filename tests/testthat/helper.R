skip_if_not_or_load_if_installed <- function(package) {
  testthat::skip_if_not_installed(package)
  suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )
}

# load hard deps to use them without namespacing
library(insight)
library(datawizard)
library(effectsize)
library(bayestestR)
library(correlation)
library(modelbased)
library(parameters)
library(performance)

suppressPackageStartupMessages(library(ggplot2))
