#' Half-violin plot
#'
#' Create a half-violin plot.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_violin
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violinhalf() +
#'   theme_modern() +
#'   scale_fill_material_d()
#' @seealso https://stackoverflow.com/questions/52034747/plot-only-one-side-half-of-the-violin-plot
#'
#' @import ggplot2
#' @export
geom_violinhalf <- function(mapping = NULL,
                            data = NULL,
                            stat = "ydensity",
                            position = "dodge",
                            trim = TRUE,
                            scale = "area",
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {
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
      ...
    )
  )
}


#' GeomViolinHalf
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @keywords internal
GeomViolinHalf <-
  ggproto("GeomViolinHalf", Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data <- do.call(rbind, lapply(split(data, data$group), function(.group) {
        .group$ymin <- min(.group$y)
        .group$ymax <- max(.group$y)
        .group$xmin <- .group$x
        .group$xmax <- .group$x + .group$width / 2
        .group
      }))
    },
    draw_group = function(data, panel_scales, coord) {
      # Find the points for the line to go all the way around
      data$xminv <- data$x
      data$xmaxv <- data$x + data$violinwidth * (data$xmax - data$x)

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
      weight = 1, colour = "grey20", fill = "white", size = 0.5,
      alpha = NA, linetype = "solid"
    ),
    required_aes = c("x", "y")
  )


#' @keywords internal
"%||%" <- function(a, b) if (!is.null(a)) a else b


#' @keywords internal
.grobName <- function(prefix, grob) {
  insight::check_if_installed("grid")

  grob$name <- grid::grobName(grob, prefix)
  grob
}
