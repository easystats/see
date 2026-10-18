#' Raincloud plot
#'
#' Create a raincloud plot, which shows, for each group, the distribution of a
#' numeric variable as a half violin (the cloud), the median with quantile
#' intervals, and the raw observations as jittered points (the rain). The
#' half violins and the intervals are drawn with [geom_violinhalf()], so this
#' function needs the **ggdist** package (version 3.3.3 or later). Each half
#' violin extends to the right of its position (up, in a horizontal plot), and
#' the points are jittered in a band on the other side, from 0.45 to 0.05 times
#' the spacing of the categorical axis away from the position (or closer, for a
#' narrow `width`). Optionally, the quantile intervals of each group can be
#' shaded behind its points, for example as a background for estimated means
#' and their confidence intervals. Dodged groups, as with `fill` mapped to a
#' second variable, are not supported: the half violins are dodged, but the
#' points and the shading of all groups at a position share one band, and the
#' shading pools their values. The categorical axis must be discrete.
#'
#' @inheritParams geom_violinhalf
#' @param interval Logical. If `TRUE` (default), add the median and the 66%
#'   and 95% quantile intervals of each group at its position, as in
#'   [geom_violinhalf()].
#' @param shade A numeric vector of coverages between 0 and 1, such as
#'   `c(0.5, 0.8, 0.95)`. For each coverage, a rectangle behind the points of
#'   each group spans the central quantile interval of the group's values
#'   ([stats::quantile()], type 7, without missing values), computed on the
#'   scale of the axis (after a transformation such as `scale_y_log10()`).
#'   Each rectangle has an alpha of 0.2, so the inner intervals, where the
#'   rectangles overlap, are shaded darker. If `NULL` (default), nothing is
#'   shaded.
#' @param seed The random seed of the jitter of the points, a single whole
#'   number. The same seed gives the same jitter.
#' @param size_points,alpha_points,color_points The size, transparency, and
#'   color of the points. If one of them is `NULL`, the points take the
#'   matching aesthetic (`size`, `alpha`, or `colour`) instead. By default,
#'   only `color_points` is `NULL`.
#' @param fill_shade The fill color of the shaded intervals.
#' @param show.legend Should the half violins be included in the legends? The
#'   points and the shading are never included.
#' @param ... Other arguments passed to [geom_violinhalf()], such as `fill`,
#'   `alpha`, `bw`, or `.width`. They do not reach the points or the shading,
#'   except for `width`, `height`, and `na.rm`. So `position` moves only the
#'   half violins and the intervals. A `width` (or `height`, in a horizontal
#'   plot) is the width of a whole violin, as in [geom_violinhalf()], so each
#'   half violin and the band of the points take half of it. `flip = TRUE` is
#'   not supported, because the half violins would cover the points.
#'
#' @return A list of ggplot2 layers.
#'
#' @examplesIf requireNamespace("ggdist", quietly = TRUE) && utils::packageVersion("ggdist") >= "3.3.3"
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_raincloud() +
#'   theme_modern()
#'
#' # A horizontal raincloud plot
#' ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
#'   geom_raincloud() +
#'   theme_modern()
#'
#' # Shade the 50%, 80%, and 95% quantile intervals behind the points
#' ggplot(iris, aes(x = Species, y = Sepal.Length, colour = Species)) +
#'   geom_raincloud(shade = c(0.5, 0.8, 0.95)) +
#'   theme_modern()
#'
#' @export
geom_raincloud <- function(
  mapping = NULL,
  data = NULL,
  trim = TRUE,
  scale = c("area", "count", "width"),
  interval = TRUE,
  shade = NULL,
  seed = 1,
  ...,
  size_points = 1.5,
  alpha_points = 0.5,
  color_points = NULL,
  fill_shade = "grey50",
  show.legend = NA,
  inherit.aes = TRUE
) {
  insight::check_if_installed("ggdist", minimum_version = "3.3.3")
  scale <- match.arg(scale)

  if (!isTRUE(interval) && !isFALSE(interval)) {
    insight::format_error("`interval` must be either `TRUE` or `FALSE`.")
  }
  if (
    !is.null(shade) &&
      (!is.numeric(shade) ||
        length(shade) == 0 ||
        anyNA(shade) ||
        any(shade <= 0 | shade >= 1))
  ) {
    insight::format_error(
      "`shade` must be `NULL` or a numeric vector of coverages between 0 and 1."
    )
  }
  if (
    !is.numeric(seed) ||
      length(seed) != 1 ||
      !is.finite(seed) ||
      seed != round(seed) ||
      abs(seed) > .Machine$integer.max
  ) {
    insight::format_error("`seed` must be a single whole number.")
  }
  extra_args <- list(...)
  if (!is.null(extra_args[["flip"]]) && !isFALSE(extra_args[["flip"]])) {
    insight::format_error(
      "`flip` is not supported, because the half violins would cover the points."
    )
  }

  # half of a whole violin's `width` (or `height`) sets the band of the points,
  # as it sets the half violins; otherwise both are 0.45 times the resolution
  half_width <- list(
    width = extra_args[["width"]],
    height = extra_args[["height"]]
  )
  half_width <- lapply(half_width, function(w) if (!is.null(w)) w / 2)

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

  points_args <- list(
    mapping = mapping,
    data = data,
    position = .position_raincloud(seed, half_width),
    show.legend = FALSE,
    inherit.aes = inherit.aes
  )
  points_args$size <- size_points
  points_args$alpha <- alpha_points
  points_args$colour <- color_points
  points_args$na.rm <- extra_args[["na.rm"]]
  points <- do.call(ggplot2::geom_point, points_args)

  if (is.null(shade)) {
    return(c(violin, list(points)))
  }

  shading <- ggplot2::layer(
    mapping = mapping,
    data = data,
    stat = .stat_raincloud_shade(shade, half_width),
    geom = "rect",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      fill = fill_shade,
      colour = NA,
      alpha = 0.2,
      na.rm = isTRUE(extra_args[["na.rm"]])
    )
  )
  c(list(shading), violin, list(points))
}


# Helpers ----------------------------------------------------------------

# The categorical axis of a raincloud: y if only y is discrete, else x
.raincloud_horizontal <- function(data) {
  inherits(data$y, "mapped_discrete") && !inherits(data$x, "mapped_discrete")
}

# The band of the points along the categorical axis, as distances from the
# position: from `half` (half of a whole violin) to `gap`
.raincloud_band <- function(data, half_width) {
  horizontal <- .raincloud_horizontal(data)
  cat_axis <- if (horizontal) "y" else "x"
  r <- ggplot2::resolution(data[[cat_axis]], zero = FALSE)
  half <- half_width[[if (horizontal) "height" else "width"]] %||% (0.45 * r)
  gap <- min(0.05 * r, half / 2)
  list(cat_axis = cat_axis, half = half, gap = gap, horizontal = horizontal)
}

# Run `code` with a random seed, and restore the random state afterwards
.with_raincloud_seed <- function(seed, code) {
  env <- globalenv()
  old <- env$.Random.seed
  on.exit(
    if (is.null(old)) {
      rm(".Random.seed", envir = env)
    } else {
      assign(".Random.seed", old, envir = env)
    },
    add = TRUE
  )
  set.seed(seed)
  code
}

# The position of the points: each point is moved along the categorical axis
# to a random place in the band beside its position, and keeps its value.
.position_raincloud <- function(seed, half_width) {
  ggplot2::ggproto(
    "PositionRaincloud",
    ggplot2::Position,
    seed = seed,
    half_width = half_width,
    compute_layer = function(self, data, params, layout) {
      if (nrow(data) == 0) {
        return(data)
      }
      band <- .raincloud_band(data, self$half_width)
      u <- .with_raincloud_seed(self$seed, stats::runif(nrow(data)))
      data[[band$cat_axis]] <- as.numeric(data[[band$cat_axis]]) -
        band$gap -
        u * (band$half - band$gap)
      data
    }
  )
}

# The stat of the shading: one rectangle per coverage and position, over the
# band of the points and the central quantile interval of the values there.
# Groups that share a position (with another discrete aesthetic mapped) share
# one band of points, so they share their shading too.
.stat_raincloud_shade <- function(coverage, half_width) {
  ggplot2::ggproto(
    "StatRaincloudShade",
    ggplot2::Stat,
    required_aes = c("x", "y"),
    coverage = coverage,
    half_width = half_width,
    compute_layer = function(self, data, params, layout) {
      if (nrow(data) == 0) {
        return(data.frame())
      }
      band <- .raincloud_band(data, self$half_width)
      cat_axis <- band$cat_axis
      val_axis <- if (band$horizontal) "x" else "y"
      positions <- as.numeric(data[[cat_axis]])
      groups <- split(data, list(data$PANEL, positions), drop = TRUE)
      out <- lapply(groups, function(d) {
        v <- d[[val_axis]][!is.na(d[[val_axis]])]
        pos <- as.numeric(d[[cat_axis]][!is.na(d[[cat_axis]])][1])
        if (length(v) == 0 || is.na(pos)) {
          return(NULL)
        }
        lo <- (1 - self$coverage) / 2
        rows <- data.frame(
          PANEL = d$PANEL[1],
          group = min(d$group),
          coverage = self$coverage,
          cat_min = pos - band$half,
          cat_max = pos - band$gap,
          val_min = stats::quantile(v, lo, type = 7, names = FALSE),
          val_max = stats::quantile(v, 1 - lo, type = 7, names = FALSE)
        )
        names(rows)[4:7] <- c(
          paste0(cat_axis, c("min", "max")),
          paste0(val_axis, c("min", "max"))
        )
        rows
      })
      out <- do.call(rbind, out)
      if (is.null(out)) {
        return(data.frame())
      }
      rownames(out) <- NULL
      out
    }
  )
}
