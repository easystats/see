#' Lucid theme
#'
#' A light, clear theme for ggplot.
#'
#' @inheritParams theme_modern
#' @inherit theme_modern note
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
#'   geom_point() +
#'   scale_color_metro() +
#'   theme_lucid()
#' @export
theme_lucid <- function(
  base_size = 11,
  base_family = "",
  plot.title.size = 1.1 * base_size,
  plot.title.face = "plain",
  plot.title.space = 1.35 * base_size,
  plot.title.position = "plot",
  legend.position = "right",
  axis.title.space = 0.9 * base_size,
  axis.text.space = base_size,
  legend.title.size = base_size,
  legend.text.size = 0.9 * base_size,
  axis.title.size = base_size,
  axis.title.face = "plain",
  axis.text.size = 0.9 * base_size,
  axis.text.angle = NULL,
  tags.size = base_size,
  tags.face = "plain",
  ...
) {
  theme_modern(
    base_size = base_size,
    base_family = base_family,
    plot.title.size = plot.title.size,
    plot.title.face = plot.title.face,
    plot.title.space = plot.title.space,
    plot.title.position = plot.title.position,
    legend.position = legend.position,
    axis.title.space = axis.title.space,
    axis.text.space = axis.text.space,
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
      axis.line.x = element_line(colour = "grey80", linewidth = base_size / 22),
      axis.line.y = element_line(colour = "grey80", linewidth = base_size / 22),
      rect = element_rect(colour = "#grey80", linewidth = base_size / 22),
      axis.text = element_text(colour = "grey50"),
      axis.title = element_text(colour = "grey30"),
      strip.background = element_rect(colour = "grey70", fill = "grey90"),
      strip.text = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30"),
      legend.text = element_text(colour = "grey30"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.minor = element_line(colour = "grey92"),
      ...
    )
}
