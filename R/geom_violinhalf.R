#' Half-violin plot
#'
#' Create a half-violin plot. The half violins are drawn with
#' [ggdist::stat_slab()], so this function needs the **ggdist** package.
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
#'   is `FALSE` and all half-violin geoms will have the flat-side on facing
#'   leftward. If `flip = TRUE`, then all flat-sides will face rightward.
#'   Optionally, a numeric vector can be supplied indicating which specific
#'   geoms should be flipped. See examples for more details.
#' @param scale If `"area"` (default), all half violins have the same area. If
#'   `"count"`, areas are scaled proportionally to the number of observations.
#'   If `"width"`, all half violins have the same maximum width.
#' @param interval Logical. If `TRUE`, add the median and the 66% and 95%
#'   quantile intervals of each group, drawn with
#'   [ggdist::stat_pointinterval()]. Default is `FALSE`.
#' @param ... Other arguments passed to [ggdist::stat_slab()], such as `colour`,
#'   `fill`, or `alpha`.
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
  interval = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  insight::check_if_installed("ggdist")
  scale <- match.arg(scale)

  if (!isTRUE(interval) && !isFALSE(interval)) {
    insight::format_error("`interval` must be either `TRUE` or `FALSE`.")
  }
  if (!identical(stat, "ydensity")) {
    insight::format_warning(
      "`stat` is ignored. The half violins are always computed by `ggdist::stat_slab()`."
    )
  }

  thickness <- switch(
    scale,
    area = quote(ggplot2::after_stat(pdf)),
    count = quote(ggplot2::after_stat(pdf * n)),
    width = quote(ggplot2::after_stat(.violinhalf_width(pdf, group)))
  )
  extra <- list(thickness = thickness)
  if (is.numeric(flip)) {
    extra$side <- bquote(ggplot2::after_stat(
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

  slab_args <- list(
    mapping = .add_to_mapping(mapping, extra),
    data = data,
    geom = .geom_violinhalf_slab(),
    position = position,
    density = ggdist::density_unbounded(bandwidth = "nrd0"),
    trim = trim,
    normalize = "all",
    scale = 0.45,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
  slab_args$side <- side
  slab <- do.call(ggdist::stat_slab, c(slab_args, list(...)))

  if (!interval) {
    return(slab)
  }

  list(
    slab,
    ggdist::stat_pointinterval(
      mapping = mapping,
      data = data,
      position = position,
      point_interval = "median_qi",
      .width = c(0.66, 0.95),
      show.legend = FALSE,
      inherit.aes = inherit.aes
    )
  )
}


# Helpers ----------------------------------------------------------------

# Scale each group's density to a maximum of 1 (`scale = "width"`). This is a
# function because `after_stat()` renames a bare `max` to `ymax`.
.violinhalf_width <- function(pdf, group) {
  pdf / stats::ave(pdf, group, FUN = function(v) max(v, na.rm = TRUE))
}


# Add aesthetics to a user mapping, unless the user already maps them.
.add_to_mapping <- function(mapping, extra) {
  aes_args <- if (is.null(mapping)) list() else as.list(mapping)
  for (name in names(extra)) {
    if (is.null(aes_args[[name]])) {
      aes_args[[name]] <- extra[[name]]
    }
  }
  do.call(ggplot2::aes, aes_args)
}


# ggdist's slab geom with the defaults of the old half-violin geom: a white
# fill and a dark grey outline. Built on call, because ggdist is only
# suggested.
.geom_violinhalf_slab <- function() {
  ggplot2::ggproto(
    "GeomViolinHalfSlab",
    ggdist::GeomSlab,
    default_aes = utils::modifyList(
      ggdist::GeomSlab$default_aes,
      list(colour = "grey20", fill = "white", linewidth = 0.5)
    )
  )
}
