#' Radar coordinate system
#'
#' Add a radar coordinate system useful for radar charts.
#'
#' @inheritParams ggplot2::coord_polar
#' @param ... Other arguments to be passed to `ggproto`.
#'
#' @examplesIf require("datawizard") && require("ggplot2")
#' # Create a radar/spider chart with ggplot:
#' data(iris)
#' data <- aggregate(iris[-5], list(Species = iris$Species), mean)
#' data <- data_to_long(
#'   data,
#'   c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
#' )
#'
#' ggplot(data, aes(x = name, y = value, color = Species, group = Species)) +
#'   geom_polygon(fill = NA, linewidth = 2) +
#'   coord_radar(start = -pi / 4)
#' @export
coord_radar <- function(theta = "x", start = 0, direction = 1, ...) {
  theta <- match.arg(theta, c("x", "y"))
  r <- ifelse(theta == "x", "y", "x")

  ggplot2::ggproto(
    "CordRadar",
    CoordPolar,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction),
    is_linear = function(coord) {
      TRUE
    },
    ...
  )
}
