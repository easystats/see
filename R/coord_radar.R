#' Radar coordinate system
#'
#' Add a radar coordinate system useful for radar charts.
#'
#' @inheritParams ggplot2::coord_polar
#' @param ... Other arguments to be passed to `ggproto`.
#'
#' @examples
#' # Create a radar/spider chart with ggplot:
#' if (require("dplyr") && require("tidyr") && require("ggplot2")) {
#'   data <- iris %>%
#'     group_by(Species) %>%
#'     summarise_all(mean) %>%
#'     pivot_longer(-Species)
#'
#'   data %>%
#'     ggplot(aes(x = name, y = value, color = Species, group = Species)) +
#'     geom_polygon(fill = NA, size = 2) +
#'     coord_radar(start = -pi / 4)
#' }
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
