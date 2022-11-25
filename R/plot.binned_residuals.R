#' @importFrom ggplot2 .data
#' @export
plot.see_binned_residuals <- function(x,
                                      size_line = 0.7,
                                      size_point = 2.2,
                                      colors = social_colors(c("blue", "red", "green")),
                                      style = theme_lucid,
                                      ...) {
  x$se.lo <- -x$se
  if (length(unique(x$group)) > 1) {
    ltitle <- "Within error bounds"
  } else {
    ltitle <- NULL
  }
  dots <- list(...)

  # set defaults
  term <- attr(x, "term", exact = TRUE)

  if (missing(style) && !is.null(attr(x, "theme"))) {
    theme_style <- unlist(strsplit(attr(x, "theme"), "::", fixed = TRUE))
    style <- get(theme_style[2], asNamespace(theme_style[1]))
  }
  theme_style <- style

  if (is.null(colors) || length(colors) != 3) {
    colors <- social_colors(c("blue", "red", "green"))
  }
  colors <- unname(colors)

  if (is.null(term)) {
    xtitle <- sprintf("Estimated Probability of %s", attr(x, "resp_var", exact = TRUE))
  } else {
    xtitle <- term
  }

  # show or hide dots - may be useful for large models with many observations
  if (isTRUE(dots[["show_dots"]])) {
    x$ybar[x$group == "yes"] <- NA
    x$CI_low[x$group == "yes"] <- NA
    x$CI_high[x$group == "yes"] <- NA
  }

  p <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data$xbar)) +
    ggplot2::geom_abline(slope = 0, intercept = 0, colour = "grey80")

  if (isTRUE(insight::check_if_installed("mgcv", quietly = TRUE))) {
    p <- p +
      ggplot2::stat_smooth(
        ggplot2::aes(y = .data$ybar),
        method = "gam",
        se = FALSE,
        formula = y ~ s(x, bs = "tp"),
        colour = colors[3],
        linewidth = size_line
      )
  }

  p <- p +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -Inf, ymax = .data$se.lo), alpha = 0.1, fill = "grey70") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$se, ymax = Inf), alpha = 0.1, fill = "grey70") +
    ggplot2::geom_line(ggplot2::aes(y = .data$se), colour = "grey70") +
    ggplot2::geom_line(ggplot2::aes(y = .data$se.lo), colour = "grey70") +
    ggplot2::scale_color_manual(values = colors[2:1]) +
    ggplot2::labs(
      x = xtitle,
      y = "Average residual",
      colour = ltitle,
      title = "Binned Residuals",
      subtitle = "Points should be within error bounds"
    )

  if (is.null(term)) {
    p <- p + ggplot2::scale_x_continuous(labels = .percents)
  }

  if (is.null(ltitle)) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(y = .data$ybar), size = size_point) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$CI_low, ymax = .data$CI_high),
        linewidth = size_line,
        width = 0
      )
  } else {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(y = .data$ybar, colour = .data$group), size = size_point) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = .data$CI_low,
          ymax = .data$CI_high,
          colour = .data$group
        ),
        linewidth = size_line,
        width = 0
      )
  }

  if (isTRUE(dots[["check_model"]])) {
    p <- p + theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    )
  }

  if (isTRUE(dots[["adjust_legend"]])) {
    p <- p + ggplot2::theme(
      legend.position = "bottom",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(-5, -5, -5, -5)
    )
  }

  p
}
