#' Blackboard dark theme
#'
#' A modern, sleek and dark theme for ggplot.
#'
#' @inheritParams theme_modern
#' @inherit theme_modern note
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
#'   geom_point(color = see_colors("lime")) +
#'   theme_blackboard()
#' @export
theme_blackboard <- function(
  base_size = 11,
  base_family = "",
  plot.title.size = 1.35 * base_size,
  plot.title.face = "plain",
  plot.title.space = 1.8 * base_size,
  plot.title.position = "plot",
  legend.position = "right",
  axis.title.space = 1.8 * base_size,
  legend.title.size = 1.2 * base_size,
  legend.text.size = 1.1 * base_size,
  axis.title.size = 1.2 * base_size,
  axis.title.face = "plain",
  axis.text.size = 1.1 * base_size,
  axis.text.angle = NULL,
  tags.size = 1.35 * base_size,
  tags.face = "bold",
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
      plot.background = element_rect(fill = "#0d0d0d"),
      panel.background = element_rect(fill = "#0d0d0d"),
      legend.background = element_rect(fill = "#0d0d0d"),
      axis.line = element_line(color = "#E0E0E0", linewidth = base_size / 22),
      rect = element_rect(colour = "#E0E0E0", linewidth = base_size / 22),
      text = element_text(color = "#E0E0E0"),
      axis.text = element_text(color = "#E0E0E0"),
      strip.text = element_text(color = "#E0E0E0"),
      ...
    )
}
