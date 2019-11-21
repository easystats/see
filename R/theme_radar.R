#' Themes for radar plots
#'
#' \code{theme_radar()} is a light, clear theme for ggplot radar-plots, while \code{theme_radar_dark()} is a dark variant of \code{theme_radar()}.
#'
#' @inheritParams theme_modern
#'
#' @seealso \code{\link{coord_radar}}
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#' library(see)
#'
#' data <- iris %>%
#'   group_by(Species) %>%
#'   summarise_all(mean) %>%
#'   pivot_longer(-Species)
#'
#' data %>%
#'   ggplot(aes(
#'     x = name,
#'     y = value,
#'     color = Species,
#'     group = Species,
#'     fill = Species
#'   )) +
#'   geom_polygon(size = 1, alpha = .1) +
#'   coord_radar() +
#'   theme_radar()
#' @export
theme_radar <- function(
  base_size = 11,
  base_family = "",
  plot.title.size = 12,
  plot.title.face = "plain",
  plot.title.space = 15,
  legend.position = "right",
  axis.title.space = 15,
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
      axis.line        = element_blank(),
      axis.text        = element_text(colour = "grey20"),
      axis.title       = element_text(colour = "grey10"),
      strip.background = element_rect(colour = "grey70", fill = "grey90"),
      strip.text       = element_text(colour = "grey20"),
      legend.title     = element_text(colour = "grey20"),
      legend.text      = element_text(colour = "grey25"),
      panel.grid.major = element_line(colour = "grey80"),
      panel.grid.minor = element_line(colour = "grey90"),
      panel.border     = element_blank()
    )
}


#' @rdname theme_radar
#' @export
theme_radar_dark <- function(
  base_size = 11,
  base_family = "",
  plot.title.size = 12,
  plot.title.face = "plain",
  plot.title.space = 15,
  legend.position = "right",
  axis.title.space = 15,
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
      axis.line = element_blank(),
      text = element_text(color = "#f2f2f2"),
      axis.text = element_text(color = "#f2f2f2"),
      panel.grid.major = element_line(color = "#465463"),
      strip.text = element_text(color = "#f2f2f2"),
      panel.border     = element_blank()
    )
}
