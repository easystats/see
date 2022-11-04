#' @export
plot.see_p_function <- function(x,
                                colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                size_line = c(0.6, 0.8),
                                grid = FALSE,
                                show_intercept = FALSE,
                                ...) {
  # data for ribbons
  data_ribbon <- attr(x, "data")

  # data for vertical CI level lines
  data_ci_segments <- x
  data_ci_segments$group <- as.factor(data_ci_segments$group)

  # remove intercept?
  data_ribbon <- .remove_intercept(data_ribbon, show_intercept = show_intercept)
  data_ci_segments <- .remove_intercept(data_ci_segments, show_intercept = show_intercept)

  pretty_names <- attributes(x)$pretty_names
  for (pn in seq_along(pretty_names)) {
    data_ribbon$Parameter[data_ribbon$Parameter == names(pretty_names[pn])] <- pretty_names[pn]
    data_ci_segments$Parameter[data_ci_segments$Parameter == names(pretty_names[pn])] <- pretty_names[pn]
  }

  # for multiple overlayed plots in different colors, use dark gray CI lines
  if (!grid && insight::n_unique(data_ribbon$Parameter) > 1) {
    colors[1] <- "#777777"
  }

  # setup - no color/fill aes for ribbons when we have no facets
  if (isTRUE(grid) || insight::n_unique(data_ribbon$Parameter) == 1) {
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
        alpha = 0.2
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
        alpha = 0.2
      )
  }

  p <- p +
    # points for vertical CI bars
    ggplot2::geom_point(
      data = data_ci_segments,
      mapping = ggplot2::aes(
        x = .data$CI_low,
        y = 1 - .data$CI,
        group = .data$Parameter
      ),
      colour = colors[1],
      size = 1.5,
      show.legend = FALSE
    ) +
    # points for vertical CI bars
    ggplot2::geom_point(
      data = data_ci_segments,
      mapping = ggplot2::aes(
        x = .data$CI_high,
        y = 1 - .data$CI,
        group = .data$Parameter
      ),
      colour = colors[1],
      size = 1.5,
      show.legend = FALSE
    ) +
    # lines for vertical CI bars
    ggplot2::geom_segment(
      data = data_ci_segments,
      mapping = ggplot2::aes(
        x = .data$CI_low,
        y = 1 - .data$CI,
        xend = .data$CI_high,
        yend = 1 - .data$CI,
        group = .data$Parameter,
        size = .data$group
      ),
      colour = colors[1],
      show.legend = FALSE
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
    ggplot2::labs(y = "P value", x = "Range of Estimates", colour = NULL) +
    theme_lucid() +
    ggplot2::scale_size_manual(values = size_line, guide = "none")

  # facets for grids, different color/fill when no grids
  if (isTRUE(grid)) {
    p <- p + ggplot2::facet_grid(~ .data$Parameter, scales = "free_x")
  } else if (insight::n_unique(data_ribbon$Parameter) > 1) {
    p <- p +
      scale_color_flat_d(guide = "none") +
      scale_fill_flat_d()
  }

  p
}
