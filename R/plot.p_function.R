#' @export
plot.see_p_function <- function(x,
                                colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                ...) {
  # CI level emphasize
  line_size <- .8

  # data for ribbons
  data_ribbon_full <- attr(x, "data")
  # data for vertical CI level lines
  data_ci_segments_full <- x
  # point estimate
  point_estimate <- attr(x, "point_estimate")

  plot_list <- lapply(unique(x$Parameter), function(param) {
    data_ribbon <- data_ribbon_full[data_ribbon_full$Parameter == param, ]
    data_ci_segments <- data_ci_segments_full[data_ci_segments_full$Parameter == param, ]
    data_ci_segments$group <- as.factor(data_ci_segments$group)

    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = data_ribbon,
        mapping = ggplot2::aes(x = .data$x, ymin = 0, ymax = 1 - .data$CI),
        color = colors[2],
        fill = colors[2],
        alpha = 0.1
      ) +
      ggplot2::geom_point(
        data = data_ci_segments,
        mapping = ggplot2::aes(x = .data$CI_low, y = 1 - .data$CI, colour = .data$group)
      ) +
      ggplot2::geom_point(
        data = data_ci_segments,
        mapping = ggplot2::aes(x = .data$CI_high, y = 1 - .data$CI, colour = .data$group)
      ) +
      ggplot2::geom_segment(
        data = data_ci_segments,
        mapping = ggplot2::aes(
          x = .data$CI_low,
          y = 1 - .data$CI,
          xend = .data$CI_high,
          yend = 1 - .data$CI,
          colour = .data$group
        ),
        size = line_size
      ) +
      # ggplot2::geom_segment(
      #   mapping = ggplot2::aes(
      #     x = point_estimate,
      #     xend = point_estimate,
      #     y = 0,
      #     yend = 1
      #   ),
      #   linetype = "dashed",
      #   color = "#cccccc"
      # ) +
      # ggplot2::annotate(
      #   geom = "text",
      #   x = point_estimate,
      #   y = 0.025,
      #   label = sprintf("%.3f", point_estimate)
      # ) +
      ggplot2::scale_color_manual(
        # values = c(colors[1], colors[3]),
        values = c(colors[1], colors[1]),
        guide = "none"
      ) +
      theme_lucid() +
      ggplot2::theme(panel.grid.minor.y = element_blank())

    # p <- p + ggplot2::labs(y = "P value", x = paste0("Range of Estimates for ", param), colour = NULL)
    if (length(unique(x$Parameter)) == 1) {
      p <- p +
        ggplot2::labs(y = "P value", x = paste0("Range of Estimates for ", param), colour = NULL) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.05),
          sec.axis = ggplot2::sec_axis(
            trans = ~ 1 - .,
            name = "Compatibility Interval",
            breaks = seq(0, 1, 0.05),
            labels = sprintf("%g%%", round(100 * seq(0, 1, 0.05)))
          ),
          expand = c(0, 0)
        )

    } else if (param == unique(x$Parameter)[1]) {
      p <- p +
        ggplot2::labs(y = "P value", x = paste0("Range of Estimates for ", param), colour = NULL) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.05),
          expand = c(0, 0)
        )

    } else if (param == unique(x$Parameter)[length(unique(x$Parameter))]) {
      p <- p +
        ggplot2::labs(y = NULL, x = paste0("Range of Estimates for ", param), colour = NULL) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.05),
          sec.axis = ggplot2::sec_axis(
            trans = ~ 1 - .,
            name = "Compatibility Interval",
            breaks = seq(0, 1, 0.05),
            labels = sprintf("%g%%", round(100 * seq(0, 1, 0.05)))
          ),
          expand = c(0, 0)
        )

    } else {
      p <- p +
        ggplot2::labs(y = NULL, x = paste0("Range of Estimates for ", param), colour = NULL) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.05),
          expand = c(0, 0)
        )
    }
    p
  })

  plots(plot_list)
}
