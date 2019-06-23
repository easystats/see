#' @rdname data_plot
#' @param point_size Size of point-geoms.
#' @importFrom rlang .data
#' @export
plot.see_bayesfactor_savagedickey <- function(x, point_size = 2, rope_color = "#0171D3", rope_alpha = .2, ...) {
  plot_data <- attr(x, "plot_data")$plot_data
  d_points <- attr(x, "plot_data")$d_points
  hypothesis <- attr(x, "hypothesis")

  # make sure point outline matches theme
  t <- theme_get()
  if (is.null(t$panel.grid.major$colour))
    null_point_outline <- "white"
  else
    null_point_outline <- t$panel.grid.major$colour

  p <- plot_data %>%
    ggplot(aes(
      x = .data$x,
      y = .data$y,
      color = .data$Distribution,
      fill = .data$Distribution
    )) +
    geom_line(size = 1) +
    geom_area(alpha = 0.15) +
    geom_vline(xintercept = hypothesis, linetype = "dashed", colour = "grey50") +
    facet_wrap(~ind, scales = "free") +
    labs(y = "Density",
         color = "Distribution",
         fill = "Distribution",
         x = "") +
    theme(legend.position = "bottom")

  if (length(hypothesis) > 1) {
    rope <- range(hypothesis)
    p <-
      p + annotate(
        "rect",
        xmin = rope[1],
        xmax = rope[2],
        ymin = 0,
        ymax = Inf,
        fill = rope_color,
        alpha = rope_alpha
      )
  } else {
    p <- p +
      geom_point(data = d_points, size = point_size, pch = 21, colour = null_point_outline, stroke = 1)
  }

  p
}
