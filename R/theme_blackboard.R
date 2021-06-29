#' Blackboard dark theme
#'
#' A modern, sleek and dark theme for ggplot.
#'
#' @inheritParams theme_modern
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
#'   geom_point(color = "white") +
#'   theme_blackboard()
#' @export
theme_blackboard <-
  function(base_size = 11,
           base_family = "",
           plot.title.size = 15,
           plot.title.face = "plain",
           plot.title.space = 20,
           plot.title.position = "plot",
           legend.position = "right",
           axis.title.space = 20,
           legend.title.size = 13,
           legend.text.size = 12,
           axis.title.size = 13,
           axis.title.face = "plain",
           axis.text.size = 12,
           axis.text.angle = NULL,
           tags.size = 15,
           tags.face = "bold") {
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
        axis.line = element_line(color = "#E0E0E0"),
        text = element_text(color = "#E0E0E0"),
        axis.text = element_text(color = "#E0E0E0"),
        strip.text = element_text(color = "#E0E0E0")
      )
  }
