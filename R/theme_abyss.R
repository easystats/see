#' Abyss theme
#'
#' A deep dark blue theme for ggplot.
#'
#' @inheritParams theme_modern
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
#'   geom_point(color = "white") +
#'   theme_abyss()
#' @export
theme_abyss <-
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
        plot.background = element_rect(fill = "#001429"),
        panel.background = element_rect(fill = "#001429"),
        legend.background = element_rect(fill = "#001429"),
        axis.line = element_line(color = "#f2f2f2"),
        text = element_text(color = "#f2f2f2"),
        axis.text = element_text(color = "#f2f2f2"),
        panel.grid.major = element_line(color = "#465463"),
        strip.text = element_text(color = "#f2f2f2")
      )
  }
