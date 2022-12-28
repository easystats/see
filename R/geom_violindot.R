#' Half-violin Half-dot plot
#'
#' Create a half-violin half-dot plot, useful for visualising the distribution
#' and the sample size at the same time.
#'
#' @inheritParams geom_violinhalf
#' @inheritParams ggplot2::geom_dotplot
#' @param position_dots Position adjustment for dots, either as a string, or the
#'   result of a call to a position adjustment function.
#' @param size_dots,dots_size Size adjustment for dots.
#' @param color_dots,dots_color Color adjustment for dots.
#' @param fill_dots,dots_fill Fill adjustment for dots.
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violindot() +
#'   theme_modern()
#' @import ggplot2
#' @export
geom_violindot <- function(mapping = NULL,
                           data = NULL,
                           trim = TRUE,
                           scale = c("area", "count", "width"),
                           show.legend = NA,
                           inherit.aes = TRUE,
                           dots_size = 0.7,
                           dots_color = NULL,
                           dots_fill = NULL,
                           binwidth = 0.05,
                           position_dots = ggplot2::position_nudge(x = -0.025, y = 0),
                           ...,
                           size_dots = dots_size,
                           color_dots = dots_color,
                           fill_dots = dots_fill) {
  scale <- match.arg(scale)

  if (is.null(color_dots) && is.null(fill_dots)) {
    dotplot <- geom_dotplot(
      binaxis = "y",
      mapping = mapping,
      data = data,
      dotsize = size_dots,
      stackdir = "down",
      binwidth = binwidth,
      position = position_dots,
      show.legend = FALSE,
      ...
    )
  } else if (!is.null(color_dots) && is.null(fill_dots)) {
    dotplot <- geom_dotplot(
      color = color_dots,
      mapping = mapping,
      data = data,
      binaxis = "y",
      dotsize = size_dots,
      stackdir = "down",
      binwidth = binwidth,
      position = position_dots,
      show.legend = FALSE,
      ...
    )
  } else if (is.null(color_dots) && !is.null(fill_dots)) {
    dotplot <- geom_dotplot(
      fill = fill_dots,
      mapping = mapping,
      data = data,
      binaxis = "y",
      dotsize = size_dots,
      stackdir = "down",
      binwidth = binwidth,
      position = position_dots,
      show.legend = FALSE,
      ...
    )
  } else {
    dotplot <- geom_dotplot(
      color = color_dots,
      fill = fill_dots,
      mapping = mapping,
      data = data,
      binaxis = "y",
      dotsize = size_dots,
      stackdir = "down",
      binwidth = binwidth,
      position = position_dots,
      show.legend = FALSE,
      ...
    )
  }



  list(
    geom_violinhalf(
      mapping = mapping,
      data = data,
      stat = "ydensity",
      position = "dodge",
      trim = trim,
      scale = scale,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      ...
    ),
    dotplot
  )
}
