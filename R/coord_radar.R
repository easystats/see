#' Radar coordinate system
#'
#' Add a radar coordinate system useful for radar charts.
#'
#' @param theta Can be 'x' or 'y'.
#' @param start Starting position. For instance, \code{-pi/4}
#' @param direction The direction of plotting. Can be 1 or -1.
#'
#' @examples
#' # Create a radar/spider chart with ggplot:
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' data <- iris %>%
#'   group_by(Species) %>%
#'   summarise_all(mean) %>%
#'   pivot_longer(-Species)
#'
#' data %>%
#'   ggplot(aes(x = name, y = value, color = Species, group = Species)) +
#'   # The polygon geom is a trick go connect endpoints
#'   geom_polygon(fill = NA, size = 2, show.legend = FALSE) +
#'   geom_line() +
#'   coord_radar(start = -pi/4)
#'
#' @export
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- ifelse(theta == "x", "y", "x")

  ggplot2::ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
                   direction = sign(direction),
                   is_linear = function(coord) TRUE)
}