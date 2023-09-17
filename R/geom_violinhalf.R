#' Half-violin plot
#'
#' Create a half-violin plot.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_violin
#' @param flip Should the half-violin plot switch directions? By default, this
#'   is `FALSE` and all half-violin geoms will have the flat-side on facing
#'   leftward. If `flip = TRUE`, then all flat-sides will face rightward.
#'   Optionally, a numeric vector can be supplied indicating which specific
#'   geoms should be flipped. See examples for more details.
#'
#' @examples
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
#' @export
geom_violinhalf <- function(mapping = NULL,
                            data = NULL,
                            stat = "ydensity",
                            position = "dodge",
                            trim = TRUE,
                            flip = FALSE,
                            scale = c("area", "count", "width"),
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {
  scale <- match.arg(scale)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomViolinHalf,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      flip = flip,
      ...
    )
  )
}


#' GeomViolinHalf
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#'
#' @keywords internal
GeomViolinHalf <- ggproto("GeomViolinHalf", Geom,
  extra_params = c("na.rm", "flip"),
  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    data <- do.call(rbind, lapply(split(data, data$group), function(.group) {
      .group$ymin <- min(.group$y)
      .group$ymax <- max(.group$y)
      .group$xmin <- .group$x
      .group$xmax <- .group$x + .group$width / 2
      .group
    }))
  },
  draw_group = function(data, panel_scales, coord, flip) {
    # Find the points for the line to go all the way around
    data$xminv <- data$x

    if (is.logical(flip)) {
      if (flip) {
        data$xmaxv <- data$x - data$violinwidth * (data$xmax - data$x)
      } else {
        data$xmaxv <- data$x + data$violinwidth * (data$xmax - data$x)
      }
    } else if (is.numeric(flip)) {
      if (unique(data$group) %in% flip) {
        data$xmaxv <- data$x - data$violinwidth * (data$xmax - data$x)
      } else {
        data$xmaxv <- data$x + data$violinwidth * (data$xmax - data$x)
      }
    }

    # Make sure it's sorted properly to draw the outline
    mindata <- maxdata <- data
    mindata$x <- mindata$xminv
    mindata <- mindata[order(mindata$y), , drop = FALSE]
    maxdata$x <- maxdata$xmaxv
    maxdata <- maxdata[order(maxdata$y, decreasing = TRUE), , drop = FALSE]
    newdata <- rbind(mindata, maxdata)

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1, ])

    .grobName("geom_violinhalf", GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },
  draw_key = draw_key_polygon,
  default_aes = aes(
    weight = 1,
    colour = "grey20",
    fill = "white",
    linewidth = 0.5,
    alpha = NA,
    linetype = "solid"
  ),
  required_aes = c("x", "y")
)

#' @keywords internal
.grobName <- function(prefix, grob) {
  insight::check_if_installed("grid")

  grob$name <- grid::grobName(grob, prefix)
  grob
}
