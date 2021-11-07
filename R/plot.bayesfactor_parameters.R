#' Plot method for Bayes Factors for a single parameter
#'
#' The `plot()` method for the `bayestestR::bayesfactor_parameters()` function.
#'
#' @param size_point Numeric specifying size of point-geoms.
#' @param rope_alpha Numeric specifying transparency level of ROPE ribbon.
#' @param rope_color Character specifying color of ROPE ribbon.
#' @param show_intercept Logical, if `TRUE`, the intercept-parameter is included
#'   in the plot. By default, it is hidden because in many cases the
#'   intercept-parameter has a posterior distribution on a very different
#'   location, so density curves of posterior distributions for other parameters
#'   are hardly visible.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_models
#'
#' @return A ggplot2-object.
#'
#' @importFrom ggplot2 .data
#' @export
plot.see_bayesfactor_parameters <- function(x,
                                            size_point = 2,
                                            rope_color = "#0171D3",
                                            rope_alpha = .2,
                                            show_intercept = FALSE,
                                            ...) {
  if ("log_BF" %in% names(x) && !"BF" %in% names(x)) {
    x$BF <- exp(x$log_BF)
  }

  plot_data <- attr(x, "plot_data")$plot_data
  d_points <- attr(x, "plot_data")$d_points
  hypothesis <- attr(x, "hypothesis")

  # if we have intercept-only models, keep at least the intercept
  intercepts_points <- which(.in_intercepts(d_points$ind))
  if (length(intercepts_points) &&
    nrow(d_points) > length(intercepts_points) &&
    !show_intercept) {
    intercepts_data <- which(.in_intercepts(plot_data$ind))
    plot_data <- plot_data[-intercepts_data, ]
    d_points <- d_points[-intercepts_points, ]
  }

  # make sure point outline matches theme
  .theme <- theme_get()
  if (is.null(.theme$panel.grid.major$colour)) {
    null_point_outline <- "white"
  } else {
    null_point_outline <- .theme$panel.grid.major$colour
  }

  p <- ggplot(
    plot_data,
    aes(
      x = .data$x,
      y = .data$y,
      color = .data$Distribution,
      fill = .data$Distribution
    )
  ) +
    geom_line(size = 1) +
    geom_area(alpha = 0.15, position = "identity") +
    geom_vline(xintercept = hypothesis, linetype = "dashed", colour = "grey50") +
    labs(
      y = "Density",
      color = "Distribution",
      fill = "Distribution",
      x = ""
    ) +
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
      geom_point(
        data = d_points,
        size = size_point,
        pch = 21,
        colour = null_point_outline,
        stroke = 1
      )
  }

  p
}


#' @export
plot.see_bayesfactor_savagedickey <- plot.see_bayesfactor_parameters
