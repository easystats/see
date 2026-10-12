#' Half-violin Half-dot plot
#'
#' Create a half-violin half-dot plot, useful for visualising the distribution
#' and the sample size at the same time. The half violins are drawn with
#' [ggdist::stat_slab()] and the dots with [ggdist::geom_dots()], so this
#' function needs the **ggdist** package.
#'
#' @inheritParams geom_violinhalf
#' @param binwidth The bin width of the dots. The default, `NA`, lets
#'   [ggdist::geom_dots()] choose a bin width that fits the dots into the space
#'   left of each half violin. A number sets a fixed bin width, in the units of
#'   the y axis.
#' @param position_dots Position adjustment for dots, either as a string, or the
#'   result of a call to a position adjustment function.
#' @param size_dots,dots_size Size adjustment for dots, passed to the
#'   `dotsize` argument of [ggdist::geom_dots()].
#' @param color_dots,dots_color Color adjustment for dots.
#' @param fill_dots,dots_fill Fill adjustment for dots.
#' @param ... Other arguments passed to [ggdist::stat_slab()] and
#'   [ggdist::geom_dots()].
#'
#' @return A list of ggplot2 layers.
#'
#' @examplesIf requireNamespace("ggdist", quietly = TRUE)
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violindot() +
#'   theme_modern()
#'
#' # The dots adapt to the scale of the data
#' ggplot(iris, aes(x = Species, y = Sepal.Length * 1000, fill = Species)) +
#'   geom_violindot(interval = TRUE) +
#'   theme_modern()
#'
#' @export
geom_violindot <- function(
  mapping = NULL,
  data = NULL,
  trim = TRUE,
  scale = c("area", "count", "width"),
  show.legend = NA,
  inherit.aes = TRUE,
  dots_size = 0.7,
  dots_color = NULL,
  dots_fill = NULL,
  binwidth = NA,
  position_dots = ggplot2::position_nudge(x = -0.025, y = 0),
  interval = FALSE,
  ...,
  size_dots = dots_size,
  color_dots = dots_color,
  fill_dots = dots_fill
) {
  insight::check_if_installed("ggdist")
  scale <- match.arg(scale)

  violin <- geom_violinhalf(
    mapping = mapping,
    data = data,
    trim = trim,
    scale = scale,
    interval = interval,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    ...
  )
  if (!interval) {
    violin <- list(violin)
  }

  dots_args <- list(
    mapping = mapping,
    data = data,
    side = "left",
    scale = 0.45,
    dotsize = size_dots,
    binwidth = binwidth,
    position = position_dots,
    show.legend = FALSE,
    inherit.aes = inherit.aes
  )
  dots_args$colour <- color_dots
  dots_args$fill <- fill_dots
  dots <- do.call(ggdist::geom_dots, c(dots_args, list(...)))

  c(violin, list(dots))
}
