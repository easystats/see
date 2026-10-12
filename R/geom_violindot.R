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
#'   `density`, `side`, `normalize`, `justification`, `fill_type`, `limits`,
#'   `p_limits`, `expand`, `breaks`, `outline_bars`, `slab_type`, `.width`,
#'   `point_interval`, `show_point`, `show_interval`, `fatten_point`, `arrow`,
#'   and the `point_` and `interval_` aesthetics) go to the half violins only.
#'   `colour` does not reach the dots when `color_dots` is set, and `fill` does
#'   not reach them when `fill_dots` is set. As for the half violins, a `width`
#'   (or `height`) is the width of a whole violin, so the dots take half of it.
#'   An argument named `position` matches `position_dots`. The dots of groups
#'   with fewer than two observations are still drawn.
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
  default_position <- missing(position_dots)
  if (default_position && isTRUE(interval)) {
    position_dots <- ggplot2::position_nudge(x = -0.06, y = 0)
  }
  # the default nudge also dodges the dots with their half violins
  if (default_position) {
    position_dots <- .position_violindots(position_dots$x, position_dots$y)
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

  # the dots take the same space as an undodged half violin: 0.45 times the axis
  # resolution, or half of a `width` (or `height`) in `...`
  extra_args <- list(...)
  names(extra_args) <- ggplot2::standardise_aes_names(names(extra_args))
  dots_size_aes <- list()
  if (is.null(extra_args$width)) {
    dots_size_aes$width <- quote(ggplot2::after_stat(
      0.45 * ggplot2::resolution(x, zero = FALSE)
    ))
  } else {
    extra_args$width <- extra_args$width / 2
  }
  if (is.null(extra_args$height)) {
    dots_size_aes$height <- quote(ggplot2::after_stat(
      0.45 * ggplot2::resolution(y, zero = FALSE)
    ))
  } else {
    extra_args$height <- extra_args$height / 2
  }

  dots_args <- list(
    mapping = .add_to_mapping(mapping, dots_size_aes),
    data = data,
    side = "left",
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
    "limits",
    "p_limits",
    "expand",
    "breaks",
    "outline_bars",
    "slab_type",
    ".width",
    "point_interval",
    "show_point",
    "show_interval",
    "fatten_point",
    "arrow"
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
    ),
    draw_slabs = function(self, s_data, panel_params, coord, orientation, ...) {
      s_data <- .violindot_reach(s_data, orientation)
      ggplot2::ggproto_parent(ggdist::GeomDots, self)$draw_slabs(
        s_data,
        panel_params,
        coord,
        orientation,
        ...
      )
    }
  )
}


# The default position of the dots. Groups that share a position are dodged
# as the half violins are (each box set to [x, x + w] first, as in
# `.violinhalf_box()`), and the dots of each dodged group go between the lower
# edge of its dodge cell and the cell's centre, which is the half of the cell
# that its half violin leaves free. A group alone at its position keeps its box
# [x - w, x]. Then the dots are nudged by `x` and `y`, but a dodged group's box
# keeps its lower edge, and its nudge toward the lower edge is at most half of
# its box. Built on call, like the geoms.
.position_violindots <- function(x = 0, y = 0) {
  ggplot2::ggproto(
    "PositionViolinDots",
    ggplot2::position_dodge(),
    x = x,
    y = y,
    # the groups are dodged in their own order, as the half violins are, and
    # ggdist's `order` aesthetic (the stacking order of the dots) is kept
    setup_data = function(self, data, params) {
      data
    },
    compute_panel = function(self, data, params, scales) {
      horizontal <- isTRUE(params$flipped_aes)
      pos_col <- if (horizontal) "y" else "x"
      lo_col <- paste0(pos_col, "min")
      hi_col <- paste0(pos_col, "max")
      pos <- data[[pos_col]]
      groups_here <- stats::ave(
        data$group,
        pos,
        FUN = function(g) length(unique(g))
      )
      data$violindot_dodged <- groups_here > 1
      data$violindot_lo <- data[[lo_col]]
      data$violindot_hi <- data[[hi_col]]
      stacking_order <- data$order
      data$order <- NULL
      data$violindot_order <- stacking_order

      if (any(data$violindot_dodged)) {
        data[[lo_col]] <- pos
        data[[hi_col]] <- pos + (data$violindot_hi - data$violindot_lo)
        # (the dodge can add columns, such as `ymax`, which are dropped)
        columns <- names(data)
        data <- ggplot2::ggproto_parent(
          ggplot2::PositionDodge,
          self
        )$compute_panel(data, params, scales)[columns]
        dodged <- data$violindot_dodged
        data[[hi_col]][dodged] <- data[[pos_col]][dodged]
        data[[lo_col]][!dodged] <- data$violindot_lo[!dodged]
        data[[hi_col]][!dodged] <- data$violindot_hi[!dodged]
      }
      dodged <- data$violindot_dodged

      # the nudge along the categorical axis, then along the value axis
      pos_nudge <- if (horizontal) self$y else self$x
      val_nudge <- if (horizontal) self$x else self$y
      shift <- rep_len(pos_nudge, nrow(data))
      if (pos_nudge < 0) {
        room <- (data[[hi_col]] - data[[lo_col]])[dodged] / 2
        shift[dodged] <- -pmin(-pos_nudge, room)
      }
      data[[pos_col]] <- data[[pos_col]] + shift
      data[[hi_col]] <- data[[hi_col]] + shift
      data[[lo_col]][!dodged] <- data[[lo_col]][!dodged] + shift[!dodged]
      if (val_nudge != 0) {
        nudge_value <- function(v) v + val_nudge
        data <- ggplot2::transform_position(
          data,
          trans_x = if (horizontal) nudge_value,
          trans_y = if (!horizontal) nudge_value
        )
      }

      data$order <- data$violindot_order
      data$violindot_box <- TRUE
      data[c("violindot_dodged", "violindot_lo", "violindot_hi")] <- NULL
      data$violindot_order <- NULL
      data
    }
  )
}


# ggdist stacks the dots of every group from its position across the narrowest
# box of the panel. With see's default position, the dots of each group fill
# their own box instead: a dodged group's free half cell, and a lone group's
# whole box.
.violindot_reach <- function(s_data, orientation) {
  if (is.null(s_data$violindot_box)) {
    return(s_data)
  }
  horizontal <- isTRUE(s_data$flipped_aes[1]) ||
    identical(orientation, "horizontal") ||
    identical(orientation, "y")
  lo <- if (horizontal) s_data$ymin else s_data$xmin
  hi <- if (horizontal) s_data$ymax else s_data$xmax
  narrowest <- min(hi - lo, na.rm = TRUE)
  if (!is.finite(narrowest) || narrowest <= 0) {
    return(s_data)
  }
  s_data$scale <- s_data$scale * (hi - lo) / narrowest
  s_data
}
