#' Half-violin plot
#'
#' Create a half-violin plot. The half violins are drawn with
#' [ggdist::stat_slab()], so this function needs the **ggdist** package
#' (version 3.3.3 or later). Each half violin is at most 0.45 x-axis units wide
#' by default.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_violin
#' @param stat Kept for backward compatibility. The density is always computed
#'   by [ggdist::stat_slab()], and any value other than `"ydensity"` is ignored
#'   with a warning.
#' @param trim If `TRUE` (default), trim the tails of the half violins to the
#'   range of the data. If `FALSE`, extend the tails as [ggplot2::geom_violin()]
#'   does.
#' @param flip Should the half-violin plot switch directions? By default, this
#'   is `FALSE` and all half-violin geoms will have the flat side facing
#'   leftward. If `flip = TRUE`, then all flat sides will face rightward.
#'   Optionally, a numeric vector can be supplied indicating which specific
#'   geoms should be flipped. See examples for more details.
#' @param scale If `"area"` (default), all half violins have the same area
#'   (before trimming the tails). If
#'   `"count"`, areas are scaled proportionally to the number of observations.
#'   If `"width"`, all half violins have the same maximum width.
#' @param interval Logical. If `TRUE`, add the median and the 66% and 95%
#'   quantile intervals of each group, drawn with
#'   [ggdist::stat_pointinterval()]. Default is `FALSE`.
#' @param ... Other arguments passed to [ggdist::stat_slab()], such as `colour`,
#'   `fill`, `alpha`, or `adjust`. As in [ggplot2::geom_violin()], `bw` sets the
#'   bandwidth (default `"nrd0"`) and `kernel` the kernel (default
#'   `"gaussian"`) of the density. Groups with fewer than two observations are
#'   not drawn.
#'
#' @return A ggplot2 layer, or a list of two layers if `interval = TRUE`.
#'
#' @examplesIf requireNamespace("ggdist", quietly = TRUE)
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
  if (!identical(stat, "ydensity")) {
    insight::format_warning(
      "`stat` is ignored. The half violins are always computed by `ggdist::stat_slab()`."
    )
  }

  extra_aes <- list(
    thickness = bquote(ggplot2::after_stat(
      .violinhalf_thickness(pdf, n, group, PANEL, .(scale))
    ))
  )
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

  # `bw` and `kernel` are the density arguments of `geom_violin()`
  extra_args <- list(...)
  # (ggdist evaluates these lazily, so they are fixed in local variables)
  bandwidth <- extra_args$bw %||% "nrd0"
  density_kernel <- extra_args$kernel %||% "gaussian"
  slab_density <- ggdist::density_unbounded(
    bandwidth = bandwidth,
    kernel = density_kernel
  )
  extra_args$bw <- NULL
  extra_args$kernel <- NULL

  # the slab and the interval share these, so that they dodge alike; an
  # argument in `...`, such as `width`, replaces the default
  shared <- list(
    data = data,
    position = position,
    density = slab_density,
    trim = trim,
    normalize = "panels",
    width = 0.45,
    height = 0.45,
    scale = 1,
    inherit.aes = inherit.aes
  )
  shared$side <- side
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

  # the interval takes the layout and density arguments, not the slab's
  # colours, so that it sits on the flat side of its own half violin
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
  list(
    slab,
    do.call(
      ggdist::stat_slabinterval,
      c(
        list(
          mapping = interval_mapping,
          show_slab = FALSE,
          point_interval = "median_qi",
          .width = c(0.66, 0.95),
          show.legend = FALSE
        ),
        interval_args
      )
    )
  )
}


# Helpers ----------------------------------------------------------------

# The thickness of each half violin for `scale`. "width" scales each group in
# each panel to a maximum of 1. Groups with fewer than two observations get no
# thickness, so they are not drawn, as in `geom_violin()`. This is a function
# because `after_stat()` renames a bare `max` to `ymax`.
.violinhalf_thickness <- function(pdf, n, group, panel, scale) {
  thickness <- switch(
    scale,
    area = pdf,
    count = pdf * n,
    width = pdf /
      stats::ave(pdf, panel, group, FUN = function(v) max(v, na.rm = TRUE))
  )
  few <- n < 2
  if (any(few)) {
    insight::format_warning(
      "Groups with fewer than two observations are not drawn."
    )
    thickness[few] <- NA
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
    draw_slabs = function(self, ...) {
      grobs <- ggplot2::ggproto_parent(ggdist::GeomSlab, self)$draw_slabs(...)
      lapply(grobs, .close_slab_outline)
    }
  )
}


# A slab grob is the fill polygon plus an outline path along the curved side.
# Give the polygon the outline's colour, width, and type (one value per
# vertex, all the same), and drop the path.
.close_slab_outline <- function(grob) {
  if (
    !inherits(grob, "gList") ||
      length(grob) != 2L ||
      !inherits(grob[[1]], "polygon")
  ) {
    return(grob)
  }
  fill_grob <- grob[[1]]
  outline <- grob[[2]]
  fill_grob$gp$col <- outline$gp$col[[1]]
  fill_grob$gp$lwd <- outline$gp$lwd[[1]]
  fill_grob$gp$lty <- outline$gp$lty[[1]]
  grid::gList(fill_grob)
}
