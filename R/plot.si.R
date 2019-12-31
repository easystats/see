#' @rdname data_plot
#' @importFrom rlang .data
#' @export
plot.see_si <- function(x, si_color = "#0171D3", si_alpha = .2, show_intercept = FALSE, ...) {
  plot_data <- attr(x, "plot_data")
  x$ind <- x$Parameter

  # if we have intercept-only models, keep at least the intercept
  intercepts_data <- which(.in_intercepts(plot_data$ind))
  if (length(intercepts_data) &&
      nrow(plot_data) > length(intercepts_data) &&
      !show_intercept) {
    intercepts_si <- which(.in_intercepts(x$ind))
    x <- x[-intercepts_si, ]
    plot_data <- plot_data[-intercepts_data,]
  }

  p <- ggplot(mapping = aes(
    x = .data$x,
    y = .data$y,
    color = .data$Distribution,
    fill = .data$Distribution
  )) +
    # distributions
    geom_line(size = 1, data = plot_data) +
    geom_area(alpha = 0.15, data = plot_data) +
    # SI
    geom_rect(
      aes(xmin = .data$CI_low, xmax = .data$CI_high),
      ymin = 0, ymax = Inf,
      data = x,
      fill = si_color, alpha = si_alpha,
      linetype = "dashed", colour = "grey50",
      inherit.aes = FALSE
    ) +
    labs(
      y = "Density",
      color = "Distribution",
      fill = "Distribution",
      x = "",
      title = paste0("Support Interval (BF = ", x$CI[1], " SI)")
    ) +
    theme(legend.position = "bottom")

  if (length(unique(plot_data$ind)) > 1) {
    p <- p + facet_wrap( ~ ind, scales = "free")
  }

  p
}