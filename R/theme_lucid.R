#' Lucid theme
#'
#' A light, clear theme for ggplot.
#'
#' @inheritParams theme_modern
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
#'   geom_point(color = "white") +
#'   theme_lucid()
#' @export
theme_lucid <- function(base_size = 11, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
      theme(
        axis.line.x      = element_line(colour = "grey80"),
        axis.line.y      = element_line(colour = "grey80"),
        axis.text        = element_text(colour = "grey50"),
        axis.title       = element_text(colour = "grey30"),
        strip.background = element_rect(colour = "grey70", fill = "grey90"),
        strip.text       = element_text(colour = "grey30"),
        legend.title     = element_text(colour = "grey30"),
        legend.text      = element_text(colour = "grey30")
      ))
}
