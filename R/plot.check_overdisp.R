#' @export
plot.see_check_overdisp <- function(x,
                                    size_line = 0.8,
                                    size_title = 12,
                                    size_axis_title = base_size,
                                    base_size = 10,
                                    colors = c("#3aaf85", "#1b6ca8"),
                                    type = 1,
                                    ...) {
  .plot_diag_overdispersion(
    x,
    style = theme_lucid,
    colors = colors,
    size_line = size_line,
    size_title = size_title,
    size_axis_title = size_axis_title,
    base_size = base_size,
    type = type
  )
}


.plot_diag_overdispersion <- function(x,
                                      theme_style = theme_lucid,
                                      size_axis_title = base_size,
                                      size_title = 12,
                                      base_size = 10,
                                      colors = c("#3aaf85", "#1b6ca8"),
                                      size_line = 0.8,
                                      type = 1,
                                      ...) {
  if (is.null(type) || type == 1) {
    p <- ggplot2::ggplot(x) +
      ggplot2::aes(x = .data$Predicted) +
      ggplot2::geom_smooth(ggplot2::aes(y = .data$V), linewidth = size_line, color = colors[2], se = FALSE) +
      ggplot2::geom_smooth(ggplot2::aes(y = .data$Res2), linewidth = size_line, color = colors[1]) +
      ggplot2::labs(
        title = "Misspecified dispersion and zero-inflation",
        subtitle = "Observed residual variance (green) should follow predicted residual variance (blue)",
        x = "Predicted mean",
        y = "Residual variance"
      ) +
      theme_style(
        base_size = base_size,
        plot.title.space = 3,
        axis.title.space = 5,
        plot.title.size = size_title,
        axis.title.size = size_axis_title
      )
  } else {
    p <- ggplot2::ggplot(x) +
      ggplot2::aes(x = .data$Predicted) +
      ggplot2::geom_point(ggplot2::aes(y = .data$StdRes)) +
      ggplot2::geom_hline(
        yintercept = c(-2, 2, -4, 4),
        linetype = c("solid", "solid", "dashed", "dashed"),
        color = c(rep(colors[1], 2), rep(colors[2], 2))
      ) +
      ggplot2::labs(
        title = "Misspecified dispersion and zero-inflation",
        subtitle = "Most points should be within solid lines, few points outside dashed lines",
        x = "Predicted mean",
        y = "Standardized resiuduals"
      ) +
      theme_style(
        base_size = base_size,
        plot.title.space = 3,
        axis.title.space = 5,
        plot.title.size = size_title,
        axis.title.size = size_axis_title
      )
  }

  p
}
