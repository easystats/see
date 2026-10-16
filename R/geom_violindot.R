#' Half-violin Half-dot plot
#'
#' Create a half-violin half-dot plot, useful for visualising the distribution
#' and the sample size at the same time. The half violins are drawn with
#' [ggdist::stat_slab()] and the dots with [ggdist::geom_dots()], so this
#' function needs the **ggdist** package (version 3.3.3 or later). When groups
#' are dodged, as with `fill` mapped to a second variable, each half violin
#' takes the right half of its dodge cell (the upper half in a horizontal plot,
#' see [geom_violinhalf()]), and, with the default `position_dots`, its dots go
#' in the other half. The dots stay in that half when the half violin is
#' flipped, so they overlap it.
#'
#' @inheritParams geom_violinhalf
#' @param binwidth The bin width of the dots. The default, `NA`, lets
#'   [ggdist::geom_dots()] choose a bin width that fits the dots into the space
#'   beside each half violin. A number sets a fixed bin width, in the units of
#'   the value axis. Dodged groups leave the dots less room, so a fixed bin
#'   width can make them overflow it, with a warning from ggdist.
#' @param position_dots Position adjustment for dots, either as a string, or the
#'   result of a call to a position adjustment function. The default nudges the
#'   dots by `x = -0.025` (`-0.06` with `interval = TRUE`). In a vertical plot,
#'   this moves them toward smaller x, away from the half violin, and in a
#'   dodged cell the nudge is at most half the room beside the half violin. In
#'   a horizontal plot, it moves them along the value axis. The default also
#'   dodges the dots with their half violins when `position` in `...` dodges
#'   them. A position that you set is used as given, so the dots are not
#'   dodged unless that position dodges them. Name this argument in full:
#'   `position` goes to the half violins.
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
#'   `position` goes to the half violins only (see [geom_violinhalf()]). With
#'   the default `position_dots`, the dots are dodged with the half violins,
#'   with the same dodge width, when `position` is `"dodge"` (the default) or
#'   made by [ggplot2::position_dodge()]. Under a position such as
#'   `"identity"`, the dots are not dodged. The dots of groups with fewer than
#'   two observations are still drawn.
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
  interval = FALSE,
  ...,
  position_dots = ggplot2::position_nudge(x = -0.025, y = 0),
  size_dots = dots_size,
  color_dots = dots_color,
  fill_dots = dots_fill
) {
  insight::check_if_installed("ggdist", minimum_version = "3.3.3")
  scale <- match.arg(scale)
  # the default position leaves a gap between the dots and the half violin,
  # wider with an interval, and dodges the dots with their half violins when
  # the half violins are dodged
  if (missing(position_dots)) {
    dodge <- .violindot_dodge(list(...)$position)
    position_dots <- .position_violindots(
      gap = if (isTRUE(interval)) 0.06 else 0.025,
      dodge = dodge$dodge,
      width = dodge$width
    )
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

  # the dots take the same space as an undodged half violin: half of a `width`
  # (or `height`) in `...`, else 0.45 times the axis resolution (set by the
  # dots geom, see `.violindot_size()`)
  extra_args <- list(...)
  names(extra_args) <- ggplot2::standardise_aes_names(names(extra_args))
  if (!is.null(extra_args$width)) {
    extra_args$width <- extra_args$width / 2
  }
  if (!is.null(extra_args$height)) {
    extra_args$height <- extra_args$height / 2
  }

  dots_args <- list(
    mapping = mapping,
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
    setup_data = function(self, data, params) {
      data <- .violindot_size(data, params)
      ggplot2::ggproto_parent(ggdist::GeomDots, self)$setup_data(data, params)
    },
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


# Without a `width` (or `height`), the dots box is 0.45 times the resolution of
# the categorical axis, as for the half violins. An axis that is not mapped is
# at 0, as in ggdist, so its box is 0.45 wide.
.violindot_size <- function(data, params) {
  for (aes in c("width", "height")) {
    if (is.null(params[[aes]]) && is.null(data[[aes]])) {
      pos <- data[[if (aes == "width") "x" else "y"]] %||% 0
      data[[aes]] <- 0.45 * ggplot2::resolution(as.numeric(pos), zero = FALSE)
    }
  }
  data
}


# Whether the half violins are dodged under the `position` given to
# geom_violindot() (none means geom_violinhalf()'s default, "dodge"), and the
# dodge width.
.violindot_dodge <- function(position) {
  if (is.null(position) || identical(position, "dodge")) {
    return(list(dodge = TRUE, width = NULL))
  }
  if (inherits(position, "PositionDodge")) {
    return(list(dodge = TRUE, width = position$width))
  }
  list(dodge = FALSE, width = NULL)
}


# The default position of the dots. Groups that share a position are dodged
# as the half violins are (each box set to [x, x + w] first, as in
# `.violinhalf_box()`), and the dots of each dodged group go between the lower
# edge of its dodge cell and the cell's centre, which is the half of the cell
# that an unflipped half violin leaves free. A group alone at its position
# keeps its box [x - w, x]. Then the dots move away from the half violin, along
# the categorical axis, by `gap` times the resolution of that axis. A dodged
# group's box keeps its lower edge, and its move is that gap times the share of
# its half cell in the lone box. Built on call, like the geoms.
.position_violindots <- function(gap = 0.025, dodge = TRUE, width = NULL) {
  ggplot2::ggproto(
    "PositionViolinDots",
    ggplot2::position_dodge(width = width),
    gap = gap,
    dodge = dodge,
    # the resolution of the categorical axis over the whole layer, as for the
    # width of the half violins
    setup_params = function(self, data) {
      params <- ggplot2::ggproto_parent(
        ggplot2::PositionDodge,
        self
      )$setup_params(data)
      pos <- data[[if (isTRUE(params$flipped_aes)) "y" else "x"]]
      params$violindot_res <- ggplot2::resolution(
        as.numeric(pos),
        zero = FALSE
      )
      params
    },
    # The dots are dodged in the order of the half violins: by a mapped `order`
    # that is constant within each group (the half violins keep it), else by
    # group. Rows with a missing value, which the half violins drop, take no
    # part. ggdist's `order` aesthetic (the stacking order of the dots) is kept.
    setup_data = function(self, data, params) {
      value <- data[[if (isTRUE(params$flipped_aes)) "x" else "y"]]
      ok <- !is.na(value)
      data$violindot_missing <- !ok
      data$violindot_order <- data$order
      if (!is.null(data$order)) {
        per_group <- stats::ave(
          xtfrm(data$order[ok]),
          data$PANEL[ok],
          data$group[ok],
          FUN = function(o) length(unique(o))
        )
        if (any(per_group > 1)) {
          data$order <- NULL
        }
      }
      dodge_order <- ggplot2::ggproto_parent(
        ggplot2::PositionDodge,
        self
      )$setup_data(data[ok, , drop = FALSE], params)$order
      data$order <- NA_integer_
      data$order[ok] <- dodge_order
      data
    },
    compute_panel = function(self, data, params, scales) {
      horizontal <- isTRUE(params$flipped_aes)
      pos_col <- if (horizontal) "y" else "x"
      lo_col <- paste0(pos_col, "min")
      hi_col <- paste0(pos_col, "max")
      pos <- data[[pos_col]]
      ok <- !data$violindot_missing
      groups_here <- rep(1, nrow(data))
      groups_here[ok] <- stats::ave(
        data$group[ok],
        pos[ok],
        FUN = function(g) length(unique(g))
      )
      data$violindot_dodged <- self$dodge & groups_here > 1
      data$violindot_lo <- data[[lo_col]]
      data$violindot_hi <- data[[hi_col]]

      if (any(data$violindot_dodged)) {
        data[[lo_col]][ok] <- pos[ok]
        data[[hi_col]][ok] <- pos[ok] +
          (data$violindot_hi - data$violindot_lo)[ok]
        # (the dodge can add columns, such as `ymax`, which are dropped)
        columns <- names(data)
        data[ok, ] <- ggplot2::ggproto_parent(
          ggplot2::PositionDodge,
          self
        )$compute_panel(data[ok, , drop = FALSE], params, scales)[columns]
        dodged <- data$violindot_dodged
        data[[hi_col]][dodged] <- data[[pos_col]][dodged]
        data[[lo_col]][!dodged] <- data$violindot_lo[!dodged]
        data[[hi_col]][!dodged] <- data$violindot_hi[!dodged]
      }
      dodged <- data$violindot_dodged

      # the gap, along the categorical axis only; in a dodged cell it shrinks
      # with the cell: by the share of the half cell in the lone box
      gap <- self$gap * params$violindot_res
      shift <- rep_len(-gap, nrow(data))
      half_cell <- (data[[hi_col]] - data[[lo_col]])[dodged]
      lone_box <- (data$violindot_hi - data$violindot_lo)[dodged]
      shift[dodged] <- -gap * half_cell / lone_box
      data[[pos_col]] <- data[[pos_col]] + shift
      data[[hi_col]] <- data[[hi_col]] + shift
      data[[lo_col]][!dodged] <- data[[lo_col]][!dodged] + shift[!dodged]

      data$order <- data$violindot_order
      data$violindot_box <- TRUE
      data[c(
        "violindot_dodged",
        "violindot_lo",
        "violindot_hi",
        "violindot_missing"
      )] <- NULL
      data$violindot_order <- NULL
      data
    }
  )
}


# ggdist stacks the dots of every group from its position across the narrowest
# box of the panel. With see's default position, the dots of each group are
# stacked across their own box instead: a dodged group's half cell beside its
# (unflipped) half violin, less the nudge, and a lone group's whole box. (ggdist
# still uses one bin width for all groups, the one that fits every group.)
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
