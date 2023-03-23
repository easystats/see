skip_if_not_or_load_if_installed <- function(package) {
  testthat::skip_if_not_installed(package)
  suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )
}

# load hard deps to use them without namespacing
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(datawizard))
suppressPackageStartupMessages(library(effectsize))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(correlation))
suppressPackageStartupMessages(library(modelbased))
suppressPackageStartupMessages(library(parameters))
suppressPackageStartupMessages(library(performance))

suppressPackageStartupMessages(library(ggplot2))
