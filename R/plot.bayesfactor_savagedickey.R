#' @export
plot.see_bayesfactor_savagedickey <- function(x, ...) {
  plot_data <- attr(x, "plot_data")
  hypothesis <- attr(x, "hypothesis")

  ggplot(plot_data, aes(x, y, color = Distribution, fill = Distribution)) +
    geom_line(size = 1) +
    geom_area(alpha = 0.15) +
    geom_vline(xintercept = hypothesis, linetype = "dashed") +
    facet_wrap( ~ ind, scales = "free") +
    labs(y = "Density") +
    theme(legend.position = "bottom")
}