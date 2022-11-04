#' @export
plot.see_p_function <- function(x,
                                colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                grid = FALSE,
                                ...) {
  # CI level emphasize
  line_size <- .8

  # data for ribbons
  data_ribbon <- attr(x, "data")

  # data for vertical CI level lines
  data_ci_segments <- x
  data_ci_segments$group <- as.factor(data_ci_segments$group)

  # setup - no color/fill aes for ribbons when we have no facets
  if (isTRUE(grid)) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = data_ribbon,
        mapping = ggplot2::aes(
          x = .data$x,
          ymin = 0,
          ymax = 1 - .data$CI,
          group = .data$Parameter
        ),
        fill = colors[2],
        colour = colors[2],
        alpha = 0.1
      )
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = data_ribbon,
        mapping = ggplot2::aes(
          x = .data$x,
          ymin = 0,
          ymax = 1 - .data$CI,
          fill = .data$Parameter,
          colour = .data$Parameter,
          group = .data$Parameter
        ),
        alpha = 0.1
      )
  }

  p <- p +
    # points for vertical CI bars
    ggplot2::geom_point(
      data = data_ci_segments,
      mapping = ggplot2::aes(
        x = .data$CI_low,
        y = 1 - .data$CI,
        colour = .data$group,
        group = .data$Parameter
      )
    ) +
    # points for vertical CI bars
    ggplot2::geom_point(
      data = data_ci_segments,
      mapping = ggplot2::aes(
        x = .data$CI_high,
        y = 1 - .data$CI,
        colour = .data$group,
        group = .data$Parameter
      )
    ) +
    # lines for vertical CI bars
    ggplot2::geom_segment(
      data = data_ci_segments,
      mapping = ggplot2::aes(
        x = .data$CI_low,
        y = 1 - .data$CI,
        xend = .data$CI_high,
        yend = 1 - .data$CI,
        colour = .data$group,
        group = .data$Parameter
      ),
      size = 0.9
    ) +
    # make sure we have two different y axes
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
    ) +
    # labelling
    ggplot2::labs(y = "P value", x = "Range of Estimates", colour = NULL, fill = NULL) +
    theme_lucid()

  # facets for grids, different color/fill when no grids
  if (isTRUE(grid)) {
    p <- p + ggplot2::facet_grid(~ .data$Parameter, scales = "free_x")
  } else {
    p <- p +
      scale_color_flat_d() +
      scale_fill_flat_d()
  }

  p
}
