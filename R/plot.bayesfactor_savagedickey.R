#' @rdname data_plot
#' @param point_size Size of point-geoms.
#' @importFrom rlang .data
#' @export
plot.see_bayesfactor_savagedickey <- function(x, point_size = 2, ...) {
  plot_data <- attr(x, "plot_data")
  hypothesis <- attr(x, "hypothesis")

  d_points <- plot_data %>%
    dplyr::group_by(.data$ind, .data$Distribution) %>%
    dplyr::summarise(y = .data$y[which.min(abs(.data$x))],
                     x = .data$x[which.min(abs(.data$x))]) %>%
    dplyr::ungroup()

  # make sure point outline matches theme
  t <- theme_get()
  if (is.null(t$panel.grid.major$colour))
    null_point_outline <- "white"
  else
    null_point_outline <- t$panel.grid.major$colour

  plot_data %>%
    ggplot(aes(
      x = .data$x,
      y = .data$y,
      color = .data$Distribution,
      fill = .data$Distribution
    )) +
    geom_line(size = 1) +
    geom_area(alpha = 0.15) +
    geom_vline(xintercept = hypothesis, linetype = "dashed") +
    geom_point(data = d_points, size = point_size, pch = 21, colour = null_point_outline) +
    facet_wrap(~ind, scales = "free") +
    labs(y = "Density",
         color = "Distribution",
         fill = "Distribution",
         x = "") +
    theme(legend.position = "bottom")
}