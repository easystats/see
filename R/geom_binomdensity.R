#' Add dot-densities for binary `y` variables
#'
#' @param data A dataframe.
#' @param x,y Characters corresponding to the x and y axis. Note that `y`
#'   must be a variable with two unique values.
#' @param scale Character specifying method of scaling the dot-densities. Can
#'   be: `'auto'` (corresponding to the square root of the proportion),
#'   `'proportion'`, `'density'` or a custom list with values for each factor
#'   level (see examples).
#' @param ... Other arguments passed to `ggdist::geom_dots`.
#'
#' @examplesIf require("ggdist")
#' library(ggplot2)
#' library(see)
#'
#' data <- iris[1:100, ]
#'
#' ggplot() +
#'   geom_binomdensity(data,
#'     x = "Sepal.Length",
#'     y = "Species",
#'     fill = "red",
#'     color = NA
#'   )
#'
#' # Different scales
#' data[1:70, "Species"] <- "setosa" # Create unbalanced proportions
#'
#' ggplot() +
#'   geom_binomdensity(data, x = "Sepal.Length", y = "Species", scale = "auto")
#' ggplot() +
#'   geom_binomdensity(data, x = "Sepal.Length", y = "Species", scale = "density")
#' ggplot() +
#'   geom_binomdensity(data, x = "Sepal.Length", y = "Species", scale = "proportion")
#' ggplot() +
#'   geom_binomdensity(data,
#'     x = "Sepal.Length", y = "Species",
#'     scale = list("setosa" = 0.4, "versicolor" = 0.6)
#'   )
#' @export
geom_binomdensity <- function(data, x, y, scale = "auto", ...) {
  insight::check_if_installed(c("ggplot2", "ggdist"))

  # Sanitize y (e.g., if levels with no values, etc.)
  if (is.factor(data[[y]]) && nlevels(data[[y]]) > 2L) {
    data[[y]] <- droplevels(data[[y]])
  }

  # Find y-axis levels
  y_levels <- levels(as.factor(data[[y]]))
  if (length(y_levels) != 2L) {
    insight::format_error("The y-variable should have exactly two levels.")
  }

  # Aesthetics
  vars <- c(x, y)
  data <- stats::na.omit(data[unique(vars)]) # Drop NaNs

  # Other parameters
  data$.side <- ifelse(data[[y]] == y_levels[1], "top", "bottom")
  data$.justification <- as.numeric(data[[y]] != y_levels[1])
  data$.scale <- .geom_binomdensity_scale(data, x, y, scale)

  # ggdist geom
  ggdist::geom_dots(
    # TODO: use tidy evaluation with `ggplot2::aes()` instead
    suppressWarnings(ggplot2::aes_string(
      x = x,
      y = y,
      side = ".side",
      justification = ".justification",
      scale = ".scale"
    )),
    data = data,
    na.rm = TRUE,
    ...
  )
}


# Utilities ---------------------------------------------------------------

.geom_binomdensity_scale <- function(data, x, y, scale = "auto") {
  prop <- prop.table(stats::xtabs(paste("~", y), data)) # Get prop table (useful later)
  if (
    length(scale) == 1 &&
      is.character(scale) &&
      scale %in% c("density", "proportion", "auto")
  ) {
    # Density instead of proportion
    if (scale == "density") {
      prop <- sapply(split(data, data[[y]]), function(df) {
        max(stats::density(df[[x]], na.rm = TRUE)$y) * nrow(df)
      })
      prop <- prop / sum(prop)
    }

    # Square-rooted proportions
    if (scale == "auto") {
      prop <- sqrt(prop) / sum(sqrt(prop))
    }

    out <- as.vector(prop[as.character(data[[y]])] * 0.9)
  } else if (length(scale) == 1 && is.character(scale) && scale %in% names(data)) {
    out <- data[[scale]]
  } else if (is.list(scale) && all(names(prop) %in% names(scale))) {
    # replace vals
    for (i in names(prop)) {
      prop[[i]] <- scale[[i]]
    }
    out <- as.vector(prop[as.character(data[[y]])] * 0.9)
  } else {
    insight::format_error("Oops, 'scale' argument wrongly specified.")
  }

  out
}
