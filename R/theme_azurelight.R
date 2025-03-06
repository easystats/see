#' Azurelight theme
#'
#' A light-blue, clear theme for ggplot with reduced usage of panel grids.
#'
#' @inheritParams theme_modern
#' @inherit theme_modern note
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' data(iris)
#'
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'   geom_point2(size = 2.5) +
#'   scale_color_social() +
#'   theme_azurelight()
#' @export
theme_azurelight <- function(base_size = 11,
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
                             ...) {
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
      plot.background = element_rect(fill = "#EEF7FF"),
      panel.background = element_rect(fill = "#EEF7FF"),
      axis.line.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(colour = "#E3EBF2"),
      panel.grid.major.y = element_line(colour = "#D7DFE5"),
      strip.background = element_rect(fill = "#A5BFCC", linewidth = 0),
      strip.text = element_text(colour = "white"),
      axis.ticks.x.bottom = element_line(linewidth = 0.8, colour = "#A5BFCC"),
      axis.line.x.bottom = element_line(color = "#A5BFCC"),
      legend.background = element_rect(fill = "#EEF7FF"),
      ...
    )
}
