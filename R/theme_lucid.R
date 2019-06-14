#' Lucid theme
#'
#' A light, clear theme for ggplot.
#'
#' @inheritParams theme_modern
#' @inheritParams ggplot2::theme_minimal
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
#'   geom_point(color = "white") +
#'   theme_lucid()
#' @export
theme_lucid <- function(
  plot.title.size = 13,
  plot.title.face = "plain",
  plot.title.space = 20,
  legend.position = "right",
  axis.title.space = 20,
  legend.title.size = 12,
  legend.text.size = 11,
  axis.title.size = 12,
  axis.title.face = "plain",
  axis.text.size = 11,
  axis.text.angle = NULL,
  tags.size = 13,
  tags.face = "plain") {

    theme_modern(
      plot.title.size = plot.title.size,
      plot.title.face = plot.title.face,
      plot.title.space = plot.title.space,
      legend.position = legend.position,
      axis.title.space = axis.title.space,
      legend.title.size = legend.title.size,
      legend.text.size = legend.text.size,
      axis.title.size = axis.title.size,
      axis.title.face = axis.title.face,
      axis.text.size = axis.text.size,
      axis.text.angle = axis.text.angle,
      tags.size = tags.size,
      tags.face = tags.face
    ) +
      theme(
        axis.line.x      = element_line(colour = "grey80"),
        axis.line.y      = element_line(colour = "grey80"),
        axis.text        = element_text(colour = "grey50"),
        axis.title       = element_text(colour = "grey30"),
        strip.background = element_rect(colour = "grey70", fill = "grey90"),
        strip.text       = element_text(colour = "grey30"),
        legend.title     = element_text(colour = "grey30"),
        legend.text      = element_text(colour = "grey30"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey92")
      )
}
