#' @rdname data_plot
#' @param point_size Size of point-geoms.
#' @importFrom rlang .data
#' @export
plot.see_bayesfactor_parameters <- function(x, point_size = 2, rope_color = "#0171D3", rope_alpha = .2, show_intercept = FALSE, ...) {
  plot_data <- attr(x, "plot_data")$plot_data
  d_points <- attr(x, "plot_data")$d_points
  hypothesis <- attr(x, "hypothesis")

  # if we have intercept-only models, keep at least the intercept
  intercepts_points <- which(d_points$ind %in% c("Intercept", "(Intercept)", "b_Intercept"))
  if (length(intercepts_points) &&
      nrow(d_points) > length(intercepts_points) &&
      !show_intercept) {
    intercepts_data <- which(plot_data$ind %in% c("Intercept", "(Intercept)", "b_Intercept"))
    plot_data <- plot_data[-intercepts_data, ]
    d_points <- d_points[-intercepts_points, ]
  }

  # make sure point outline matches theme
  .theme <- theme_get()
  if (is.null(.theme$panel.grid.major$colour))
    null_point_outline <- "white"
  else
    null_point_outline <- .theme$panel.grid.major$colour

  p <- plot_data %>%
    ggplot(aes(
      x = .data$x,
      y = .data$y,
      color = .data$Distribution,
      fill = .data$Distribution
    )) +
    geom_line(size = 1) +
    geom_area(alpha = 0.15, position = "identity") +
    geom_vline(xintercept = hypothesis, linetype = "dashed", colour = "grey50") +
    labs(y = "Density",
         color = "Distribution",
         fill = "Distribution",
         x = "") +
    theme(legend.position = "bottom")

  if (length(unique(plot_data$ind)) > 1) {
    p <- p + facet_wrap(~ind, scales = "free")
  }

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


#' @export
plot.see_bayesfactor_savagedickey <- plot.see_bayesfactor_parameters