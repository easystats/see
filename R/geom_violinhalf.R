#' Half-violin plot
#'
#' Create a half-violin plot. The half violins are drawn with
#' [ggdist::stat_slab()], so this function needs the **ggdist** package
#' (version 3.3.3 or later). By default, each half violin is at most 0.45
#' times the spacing of the categorical axis wide (0.45 units on a discrete
#' axis), as before. When groups are dodged, as with `fill` mapped to a second
#' variable, each group takes half of its dodge cell, from the cell's centre
#' toward its side, so that half violins do not overlap.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_violin
#' @param stat Kept for backward compatibility. The density is always computed
#'   by [ggdist::stat_slab()], and any value other than `"ydensity"` or
#'   [ggplot2::StatYdensity] is ignored with a warning.
#' @param trim If `TRUE` (default), trim the tails of the half violins to the
#'   range of the data. If `FALSE`, extend the tails as [ggplot2::geom_violin()]
#'   does.
#' @param flip Should the half-violin plot switch directions? By default, this
#'   is `FALSE` and each half violin extends to the right of its position (up,
#'   in a horizontal plot), with its flat side on the left. If `flip = TRUE`,
#'   all half violins extend to the left (down).
#'   Optionally, a numeric vector can be supplied indicating which specific
#'   geoms should be flipped. See examples for more details.
#' @param scale If `"area"` (default), all half violins have the same area
#'   (before trimming the tails). If `"count"`, areas are scaled
#'   proportionally to the number of observations. If `"width"`, all half
#'   violins have the same maximum width.
#' @param interval Logical. If `TRUE`, add the median and the 66% and 95%
#'   quantile intervals of each group, drawn with [ggdist::stat_slabinterval()]
#'   without the slab, as in [ggdist::stat_pointinterval()]. Default is
#'   `FALSE`.
#' @param ... Other arguments passed to [ggdist::stat_slab()], such as `colour`,
#'   `fill`, `alpha`, or `adjust`. As in [ggplot2::geom_violin()]:
#'   - `bw` sets the bandwidth of the density (default `"nrd0"`): a number, or
#'     one of `"nrd0"`, `"nrd"`, `"ucv"`, `"bcv"`, `"SJ"`, or `"dpi"`.
#'   - `kernel` sets the kernel (default `"gaussian"`).
#'   - `bounds` sets known bounds of the data, which must contain all values.
#'   - `width` (or `height`, in a horizontal plot) is the width of a whole
#'     violin, so each half violin is half as wide.
#'
#'   `drop` has no effect. The half violins of groups with fewer than two
#'   observations, or with all values equal, are not drawn, with a warning, but
#'   they keep their space when dodged, and their intervals are drawn.
#'   `.width`, `point_interval`, `show_point`, `show_interval`, `fatten_point`,
#'   `arrow`, and the `point_` and `interval_` aesthetics, such as `point_size`,
#'   go to the interval.
#'
#' @return A ggplot2 layer, or a list of two layers if `interval = TRUE`.
#'
#' @examplesIf requireNamespace("ggdist", quietly = TRUE) && utils::packageVersion("ggdist") >= "3.3.3"
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violinhalf() +
#'   theme_modern() +
#'   scale_fill_material_d()
#'
#' # To flip all half-violin geoms, use `flip = TRUE`:
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violinhalf(flip = TRUE) +
#'   theme_modern() +
#'   scale_fill_material_d()
#'
#' # To flip the half-violin geoms for the first and third groups only
#' # by passing a numeric vector
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violinhalf(flip = c(1, 3)) +
#'   theme_modern() +
#'   scale_fill_material_d()
#'
#' # To add the median and quantile intervals
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violinhalf(interval = TRUE) +
#'   theme_modern() +
#'   scale_fill_material_d()
#'
#' @export
geom_violinhalf <- function(
  mapping = NULL,
  data = NULL,
  stat = "ydensity",
  position = "dodge",
  trim = TRUE,
  flip = FALSE,
  scale = c("area", "count", "width"),
  show.legend = NA,
  inherit.aes = TRUE,
  interval = FALSE,
  ...
) {
  insight::check_if_installed("ggdist", minimum_version = "3.3.3")
  scale <- match.arg(scale)

  if (!isTRUE(interval) && !isFALSE(interval)) {
    insight::format_error("`interval` must be either `TRUE` or `FALSE`.")
  }
  if (!identical(stat, "ydensity") && !inherits(stat, "StatYdensity")) {
    insight::format_warning(
      "`stat` is ignored. The half violins are always computed by `ggdist::stat_slab()`."
    )
  }

  extra_args <- list(...)
  extra_aes <- list(
    thickness = bquote(ggplot2::after_stat(
      .violinhalf_thickness(pdf, n, group, PANEL, .(scale))
    ))
  )
  # each half violin is at most 0.45 times the resolution of the categorical
  # axis wide, as before; a `width` (or `height`) in `...` is the width of a
  # whole violin, as in `geom_violin()`, so the half violin gets half of it
  half_size <- list()
  if (is.null(extra_args$width)) {
    extra_aes$width <- quote(ggplot2::after_stat(
      0.45 * ggplot2::resolution(x, zero = FALSE)
    ))
  } else {
    half_size$width <- extra_args$width / 2
  }
  if (is.null(extra_args$height)) {
    extra_aes$height <- quote(ggplot2::after_stat(
      0.45 * ggplot2::resolution(y, zero = FALSE)
    ))
  } else {
    half_size$height <- extra_args$height / 2
  }
  extra_args$width <- NULL
  extra_args$height <- NULL
  if (is.numeric(flip)) {
    extra_aes$side <- bquote(ggplot2::after_stat(
      ifelse(group %in% .(flip), "left", "right")
    ))
    side <- NULL
  } else if (isTRUE(flip) || isFALSE(flip)) {
    side <- if (flip) "left" else "right"
  } else {
    insight::format_error(
      "`flip` must be either `TRUE`, `FALSE`, or a numeric vector of groups."
    )
  }
  interval_mapping <- .add_to_mapping(
    mapping,
    extra_aes[names(extra_aes) != "thickness"]
  )
  mapping <- .add_to_mapping(mapping, extra_aes)

  # `bw`, `kernel`, and `bounds` are the density arguments of `geom_violin()`
  # (ggdist evaluates these lazily, so they are fixed in local variables);
  # `drop` has no effect, because small groups are never drawn
  bandwidth <- extra_args$bw %||% "nrd0"
  density_kernel <- extra_args$kernel %||% "gaussian"
  density_bounds <- extra_args$bounds
  slab_density <- if (is.null(density_bounds)) {
    ggdist::density_unbounded(bandwidth = bandwidth, kernel = density_kernel)
  } else {
    ggdist::density_bounded(
      bounds = density_bounds,
      bandwidth = bandwidth,
      kernel = density_kernel
    )
  }
  extra_args[c("bw", "kernel", "bounds", "drop")] <- NULL

  # styles of the interval go to the interval only
  to_interval <- grepl(
    "^(\\.width|point_interval|point_.+|interval_.+|show_point|show_interval|fatten_point|arrow)$",
    names(extra_args)
  )
  interval_extra <- extra_args[to_interval]
  extra_args <- extra_args[!to_interval]

  # the slab and the interval share these, so that they dodge alike; an
  # argument in `...`, such as `normalize` or `density`, replaces the default,
  # and half of a `width` or `height` in `...` replaces the default size
  shared <- list(
    data = data,
    position = position,
    density = slab_density,
    trim = trim,
    normalize = "panels",
    scale = 1,
    inherit.aes = inherit.aes
  )
  shared$side <- side
  shared[names(half_size)] <- half_size
  shared[names(extra_args)] <- extra_args

  slab <- do.call(
    ggdist::stat_slab,
    c(
      list(
        mapping = mapping,
        geom = .geom_violinhalf_slab(),
        show.legend = show.legend
      ),
      shared
    )
  )

  if (!interval) {
    return(slab)
  }

  # the interval shares the slab's position and width, so that it dodges with
  # its own half violin and sits on its flat side, but not the slab's colours
  interval_args <- shared[intersect(
    names(shared),
    c(
      "data",
      "position",
      "density",
      "trim",
      "normalize",
      "width",
      "height",
      "scale",
      "side",
      "inherit.aes",
      "na.rm",
      "orientation",
      "adjust",
      "n"
    )
  )]
  interval_base <- list(
    mapping = interval_mapping,
    geom = .geom_violinhalf_interval(),
    show_slab = FALSE,
    point_interval = "median_qi",
    .width = c(0.66, 0.95),
    show.legend = FALSE
  )
  interval_base[names(interval_extra)] <- interval_extra
  list(
    slab,
    do.call(ggdist::stat_slabinterval, c(interval_base, interval_args))
  )
}


# Helpers ----------------------------------------------------------------

# The thickness of each half violin for `scale`. "width" scales each group in
# each panel to a maximum of 1. Groups with fewer than two observations, or
# with all values equal (an infinite density), get no thickness, so they are
# not drawn. This is a function because `after_stat()` renames a bare `max` to
# `ymax`.
.violinhalf_thickness <- function(pdf, n, group, panel, scale) {
  # (ggdist's interval rows, if any, have pdf NA and do not count)
  no_spread <- stats::ave(as.numeric(is.infinite(pdf)), panel, group, FUN = max)
  skip <- n < 2 | no_spread > 0
  pdf[skip] <- NA
  thickness <- switch(
    scale,
    area = pdf,
    count = pdf * n,
    width = pdf /
      stats::ave(pdf, panel, group, FUN = function(v) {
        if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
      })
  )
  if (any(skip)) {
    insight::format_warning(
      "Groups with fewer than two observations, or with all values equal, are not drawn."
    )
  }
  thickness
}


# Add aesthetics to a user mapping, unless the user already maps them. The
# added aesthetics are built here, so that they find see's helpers.
.add_to_mapping <- function(mapping, extra) {
  added <- do.call(ggplot2::aes, extra)
  if (is.null(mapping)) {
    return(added)
  }
  # anything else, such as a data frame passed first, is left to ggplot2's
  # own check of `mapping`
  if (!inherits(mapping, "uneval")) {
    return(mapping)
  }
  for (name in names(added)) {
    if (is.null(mapping[[name]])) {
      mapping[[name]] <- added[[name]]
    }
  }
  mapping
}


# ggdist's slab geom with the defaults of the old half-violin geom: a white
# fill and a dark grey outline around the whole half violin. ggdist outlines
# only the curved side, so the outline style moves to the border of the fill
# polygon. Built on call, because ggdist is only suggested.
.geom_violinhalf_slab <- function() {
  ggplot2::ggproto(
    "GeomViolinHalfSlab",
    ggdist::GeomSlab,
    default_aes = utils::modifyList(
      ggdist::GeomSlab$default_aes,
      list(colour = "grey20", fill = "white", linewidth = 0.5)
    ),
    setup_data = function(self, data, params) {
      data <- ggplot2::ggproto_parent(ggdist::GeomSlab, self)$setup_data(
        data,
        params
      )
      .violinhalf_box(data)
    },
    draw_slabs = function(self, s_data, panel_params, coord, orientation, ...) {
      s_data <- .violinhalf_extent(s_data, orientation)
      grobs <- ggplot2::ggproto_parent(ggdist::GeomSlab, self)$draw_slabs(
        s_data,
        panel_params,
        coord,
        orientation,
        ...
      )
      lapply(grobs, .close_slab_outline)
    }
  )
}


# ggdist's slab-and-interval geom with the same box as the half violins, so
# that the interval dodges with its own half violin. Built on call, because
# ggdist is only suggested.
.geom_violinhalf_interval <- function() {
  ggplot2::ggproto(
    "GeomViolinHalfInterval",
    ggdist::GeomSlabinterval,
    setup_data = function(self, data, params) {
      data <- ggplot2::ggproto_parent(
        ggdist::GeomSlabinterval,
        self
      )$setup_data(
        data,
        params
      )
      .violinhalf_box(data)
    }
  )
}


# As in see 0.14, every half violin gets the box [x, x + width] before
# dodging, whichever its side, so that flipped and unflipped groups at one
# position are dodged into cells alike. (ggdist puts a left-side box at
# [x - width, x], and ggplot2 dodges boxes with different xmin apart.)
.violinhalf_box <- function(data) {
  horizontal <- isTRUE(data$flipped_aes[1])
  if (horizontal) {
    size <- data$ymax - data$ymin
    data$ymin <- data$y
    data$ymax <- data$y + size
  } else {
    size <- data$xmax - data$xmin
    data$xmin <- data$x
    data$xmax <- data$x + size
  }
  data
}


# ggdist draws each slab from its position across the narrowest box of the
# panel. As in see 0.14, each half violin reaches only from its position to
# the edge of its own box (xmax - x), which is half of its dodge cell when
# groups are dodged, and the whole box when they are not.
.violinhalf_extent <- function(s_data, orientation) {
  horizontal <- isTRUE(s_data$flipped_aes[1]) ||
    identical(orientation, "horizontal") ||
    identical(orientation, "y")
  pos <- if (horizontal) s_data$y else s_data$x
  lo <- if (horizontal) s_data$ymin else s_data$xmin
  hi <- if (horizontal) s_data$ymax else s_data$xmax
  narrowest <- min(hi - lo, na.rm = TRUE)
  s_data$scale <- s_data$scale * (hi - pos) / narrowest
  s_data
}


# A slab grob is the fill polygon plus an outline path along the curved side.
# Give the polygon the outline's colour, width, and type (one value per vertex,
# all the same), and drop the path. The colour loses any transparency, so that
# the `alpha` aesthetic fades only the fill, as in `geom_violin()` (unlike
# `geom_violin()`, it also drops transparency that is part of the colour). A
# slab drawn in several pieces, or with a gradient fill, keeps ggdist's outline
# along the curved side only, so that no border runs across it.
.close_slab_outline <- function(grob) {
  if (
    !inherits(grob, "gList") ||
      length(grob) != 2L ||
      !inherits(grob[[1]], "polygon") ||
      length(unique(grob[[1]]$id)) > 1L ||
      length(unique(grob[[1]]$gp$fill)) > 1L
  ) {
    return(grob)
  }
  fill_grob <- grob[[1]]
  outline <- grob[[2]]
  outline_colour <- outline$gp$col[[1]]
  if (!is.na(outline_colour)) {
    rgba <- grDevices::col2rgb(outline_colour, alpha = TRUE)
    # a fully transparent colour, such as "transparent", stays invisible
    outline_colour <- if (rgba["alpha", 1] == 0) {
      NA_character_
    } else {
      grDevices::rgb(t(rgba[1:3, , drop = FALSE]), maxColorValue = 255)
    }
  }
  fill_grob$gp$col <- outline_colour
  fill_grob$gp$lwd <- outline$gp$lwd[[1]]
  fill_grob$gp$lty <- outline$gp$lty[[1]]
  grid::gList(fill_grob)
}
