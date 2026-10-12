#' Half-violin Half-dot plot
#'
#' Create a half-violin half-dot plot, useful for visualising the distribution
#' and the sample size at the same time. The half violins are drawn with
#' [ggdist::stat_slab()] and the dots with [ggdist::geom_dots()], so this
#' function needs the **ggdist** package (version 3.3.3 or later).
#'
#' @inheritParams geom_violinhalf
#' @param binwidth The bin width of the dots. The default, `NA`, lets
#'   [ggdist::geom_dots()] choose a bin width that fits the dots into the space
#'   beside each half violin. A number sets a fixed bin width, in the units of
#'   the value axis.
#' @param position_dots Position adjustment for dots, either as a string, or the
#'   result of a call to a position adjustment function.
#' @param size_dots,dots_size Size adjustment for dots, passed to the
#'   `dotsize` argument of [ggdist::geom_dots()].
#' @param color_dots,dots_color Color adjustment for dots.
#' @param fill_dots,dots_fill Fill adjustment for dots.
#' @param ... Other arguments passed to [geom_violinhalf()] and to
#'   [ggdist::geom_dots()]. The arguments of the density, the layout, and the
#'   interval (`flip`, `stat`, `bw`, `kernel`, `bounds`, `drop`, `adjust`, `n`,
#'   `density`, `side`, `normalize`, `justification`, `fill_type`, `.width`,
#'   `point_interval`, and the `point_` and `interval_` aesthetics) go to the
#'   half violins only. `colour` and `fill` do not reach the dots when
#'   `color_dots` or `fill_dots` is set. An argument named `position` matches
#'   `position_dots`. The dots of groups with fewer than two observations are
#'   still drawn.
#'
#' @return A list of ggplot2 layers.
#'
#' @examplesIf requireNamespace("ggdist", quietly = TRUE) && utils::packageVersion("ggdist") >= "3.3.3"
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
  insight::check_if_installed("ggdist", minimum_version = "3.3.3")
  scale <- match.arg(scale)
  # leave room for the interval between the dots and the half violin
  if (missing(position_dots) && isTRUE(interval)) {
    position_dots <- ggplot2::position_nudge(x = -0.06, y = 0)
  }

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
    width = 0.45,
    height = 0.45,
    scale = 1,
    dotsize = size_dots,
    binwidth = binwidth,
    position = position_dots,
    show.legend = FALSE,
    inherit.aes = inherit.aes
  )
  dots_args$colour <- color_dots
  dots_args$fill <- fill_dots

  # `...` reaches the dots too, except for arguments of the half violins only
  # and for the colours and position that the dot arguments set
  extra_args <- list(...)
  names(extra_args) <- ggplot2::standardise_aes_names(names(extra_args))
  half_violin_only <- c(
    "flip",
    "position",
    "stat",
    "bw",
    "kernel",
    "bounds",
    "drop",
    "adjust",
    "n",
    "density",
    "side",
    "normalize",
    "justification",
    "fill_type",
    ".width",
    "point_interval"
  )
  extra_args <- extra_args[
    !names(extra_args) %in% half_violin_only &
      !grepl("^(point|interval)_", names(extra_args))
  ]
  if (!is.null(color_dots)) {
    extra_args$colour <- NULL
  }
  if (!is.null(fill_dots)) {
    extra_args$fill <- NULL
  }
  dots_args[names(extra_args)] <- extra_args
  dots <- do.call(ggdist::geom_dots, dots_args)
  dots$geom <- .geom_violindot_dots()

  c(violin, list(dots))
}


# ggdist's dots geom with the outline of the old dot plot: black. Built on
# call, because ggdist is only suggested.
.geom_violindot_dots <- function() {
  ggplot2::ggproto(
    "GeomViolinDots",
    ggdist::GeomDots,
    default_aes = utils::modifyList(
      ggdist::GeomDots$default_aes,
      list(colour = "black")
    )
  )
}
