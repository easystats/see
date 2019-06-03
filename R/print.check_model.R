#' @importFrom rlang .data
#' @importFrom graphics plot
#' @export
print.see_check_model <- function(x, ...) {
  p <- list()
  panel <- attr(x, "panel")

  if ("VIF" %in% names(x)) p$VIF <- .plot_diag_vif(x$VIF)
  if ("QQ" %in% names(x)) p$QQ <- .plot_diag_qq(x$QQ)
  if ("NORM" %in% names(x)) p$NORM <- .plot_diag_norm(x$NORM)

  if (panel) {
    do.call(plots, p)
  } else {
    suppressWarnings(grpahics::plot(p))
  }
}



.plot_diag_vif <- function(x) {
  ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$group)) +
    geom_col(width = 0.7) +
    labs(title = "Check for Multicollinearity", x = NULL, y = NULL, fill = NULL) +
    scale_fill_manual(values = unname(flat_colors("green", "orange", "red"))) +
    theme_bw()
}



.plot_diag_norm <- function(x) {
  ggplot(x, aes(x = .data$x)) +
    geom_ribbon(
      mapping = aes(ymin = 0, ymax = .data$y),
      colour = unname(flat_colors("blue grey")),
      fill = unname(flat_colors("light blue")),
      alpha = 0.2
    ) +
    geom_line(
      mapping = aes(y = .data$curve),
      colour = unname(flat_colors("blue")),
      size = 0.8
    ) +
    labs(
      x = "Residuals",
      y = "Density",
      title = "Non-Normality of Residuals",
      subtitle = "Distribution should look like a normal curve"
    ) +
    theme_bw()
}



.plot_diag_qq <- function(x) {
  ggplot(x, aes(x = .data$x, .data$y)) +
    geom_point2(colour = unname(flat_colors("blue grey"))) +
    stat_smooth(method = "lm", se = FALSE, size = .9, colour = unname(flat_colors("blue"))) +
    labs(
      title = "Non-normality of Residuals and Outliers",
      subtitle = "Dots should be plotted along the line",
      y = "(Studentized) Residuals",
      x = "Theoretical quantiles (predicted values)"
    ) +
    theme_bw()
}