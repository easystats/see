#' The easystats' minimal theme
#'
#' A modern, sleek and elegant theme for ggplot.
#'
#' @inheritParams ggplot2::theme
#' @inheritParams ggplot2::theme_classic
#' @param plot.title.size Title size in pts. Can be "none".
#' @param plot.title.face Title font face ("plain", "italic", "bold", "bold.italic").
#' @param plot.title.space Title spacing.
#' @param plot.title.position Alignment of the plot title/subtitle and caption.
#'   The setting for plot.title.position applies to both the title and the
#'   subtitle. A value of "panel" (the default) means that titles and/or caption
#'   are aligned to the plot panels. A value of "plot" means that titles and/or
#'   caption are aligned to the entire plot (minus any space for margins and
#'   plot tag).
#' @param legend.title.size Legend elements text size in pts.
#' @param legend.text.size Legend elements text size in pts. Can be "none".
#' @param axis.title.space Axis title spacing.
#' @param axis.title.size Axis title text size in pts.
#' @param axis.title.face Axis font face ("plain", "italic", "bold", "bold.italic").
#' @param axis.text.size Axis text size in pts.
#' @param axis.text.angle Rotate  the x axis labels.
#' @param tags.size Tags text size in pts.
#' @param tags.face Tags font face ("plain", "italic", "bold", "bold.italic").
#' @param show.ticks Logical, if `TRUE`, adds inner tick marks to the plot and
#'   slightly increases the padding between axis and the related labels.
#' @param ... Further arguments passed to `ggplot2::theme()`.
#'
#' @note Base elements like plot borders, titles etc. are scaling with
#' `base_size`, i.e. when `base_size` is increased, all other relevant elements
#' are increased in proportion to the base size, to ensure proper scaling of
#' the plot. Set arguments for the related elements explicitly to define custom
#' sizes.
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_see() +
#'   theme_modern()
#'
#' # for a slightly better orientation, tick marks can be added
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_see() +
#'   theme_modern(show.ticks = TRUE)
#' @export
theme_modern <- function(
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
  show.ticks = FALSE,
  ...
) {
  # Remove legend title if necessary
  if (is.null(plot.title.size)) {
    plot.title.size <- element_text(
      size = plot.title.size,
      face = plot.title.face,
      margin = margin(0, 0, plot.title.space, 0)
    )
  } else if (plot.title.size == "none") {
    plot.title.size <- element_blank()
  } else {
    plot.title.size <- element_text(
      size = plot.title.size,
      face = plot.title.face,
      margin = margin(0, 0, plot.title.space, 0)
    )
  }

  # Remove legend title if necessary
  if (is.null(legend.title.size)) {
    legend.title.size <- element_text(size = legend.title.size)
  } else if (legend.title.size == "none") {
    legend.title.size <- element_blank()
  } else {
    legend.title.size <- element_text(size = legend.title.size)
  }

  # Remove axis title if necessary
  if (is.null(axis.title.size)) {
    axis.title.size <- element_text(
      size = axis.title.size,
      face = axis.title.face
    )
  } else if (axis.title.size == "none") {
    axis.title.size <- element_blank()
  } else {
    axis.title.size <- element_text(
      size = axis.title.size,
      face = axis.title.face
    )
  }

  # Remove axis text if necessary
  if (is.null(axis.text.size)) {
    axis.text.size <- element_text(size = axis.text.size)
  } else if (axis.text.size == "none") {
    axis.text.size <- element_blank()
  } else {
    axis.text.size <- element_text(size = axis.text.size)
  }

  # Rotate
  if (is.null(axis.text.angle)) {
    hjust <- NULL
  } else {
    hjust <- 1
  }

  # set up further arguments

  theme_args <- list(
    plot.title = plot.title.size,
    plot.title.position = plot.title.position,
    legend.position = legend.position,
    legend.text = element_text(size = legend.text.size),
    legend.title = legend.title.size,
    legend.key = element_blank(),
    legend.spacing.x = unit(2, "pt"),
    axis.title.y = element_text(
      margin = margin(t = 0, r = axis.title.space, b = 0, l = 0)
    ),
    axis.title.x = element_text(
      margin = margin(t = axis.title.space, r = 0, b = 0, l = 0)
    ),
    axis.title = axis.title.size,
    axis.text.x = element_text(angle = axis.text.angle, hjust = hjust),
    axis.text = axis.text.size,
    plot.tag = element_text(size = tags.size, face = tags.face),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

  # show ticks?
  if (show.ticks) {
    theme_args$axis.ticks.length <- unit(-0.25, "cm")
    theme_args$axis.text.x.bottom <- element_text(
      margin = margin(t = base_size * 1.3, r = 0, b = 0, l = 0)
    )
    theme_args$axis.text.y.left <- element_text(
      margin = margin(t = 0, r = base_size * 1.3, b = 0, l = 0)
    )
  } else {
    theme_args$axis.ticks <- element_blank()
  }

  theme_classic(base_size = base_size, base_family = base_family) +
    do.call(theme, c(theme_args, ...))
}
