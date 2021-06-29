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
theme_lucid <- function(base_size = 11,
                        base_family = "",
                        plot.title.size = 12,
                        plot.title.face = "plain",
                        plot.title.space = 15,
                        plot.title.position = "plot",
                        legend.position = "right",
                        axis.title.space = 10,
                        legend.title.size = 11,
                        legend.text.size = 10,
                        axis.title.size = 11,
                        axis.title.face = "plain",
                        axis.text.size = 10,
                        axis.text.angle = NULL,
                        tags.size = 11,
                        tags.face = "plain") {
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
      axis.line.x = element_line(colour = "grey80"),
      axis.line.y = element_line(colour = "grey80"),
      axis.text = element_text(colour = "grey50"),
      axis.title = element_text(colour = "grey30"),
      strip.background = element_rect(colour = "grey70", fill = "grey90"),
      strip.text = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30"),
      legend.text = element_text(colour = "grey30"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.minor = element_line(colour = "grey92")
    )
}
