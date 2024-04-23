#' \code{see}
#'
#' @title see: Framework for Easy Statistical Modeling, Visualization, and Reporting
#'
#' @description
#'
#' Provides plotting utilities supporting packages in the 'easystats' ecosystem
#' (<https://github.com/easystats/easystats>) and some extra themes, geoms, and
#' scales for 'ggplot2'. Color scales are based on
#' <https://materialui.co/>.
#'
#' References: Luedecke et al. (2021) \doi{10.21105/joss.03393}.
#'
#' @docType package
#' @aliases see see-package
#' @name see-package
#' @keywords internal
"_PACKAGE"

## see namespace: start
##
#' @import ggplot2
#'
## see namespace: end
NULL

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
.ignore_unused_imports <- function() {
  correlation::correlation()
  modelbased::estimate_expectation()
  performance::model_performance()
}
