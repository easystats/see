#' Plot method for support intervals
#'
#' The `plot()` method for the `bayestestR::si()`.
#'
#' @param si_alpha Numeric value specifying Transparency level of SI ribbon.
#' @param si_color Character specifying color of SI ribbon.
#' @param support_only Logical. Decides whether to plot only the support data,
#'   or show the "raw" prior and posterior distributions? Only applies when
#'   plotting [bayestestR::si()].
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#'
#' @return A ggplot2-object.
#'
#' @examplesIf require("rstanarm")
#' \donttest{
#' library(rstanarm)
#' library(bayestestR)
#' set.seed(123)
#' m <<- stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0)
#' result <- si(m)
#' result
#' plot(result)
#' }
#' @importFrom ggplot2 .data
#' @export
plot.see_si <- function(x,
                        si_color = "#0171D3",
                        si_alpha = 0.2,
                        show_intercept = FALSE,
                        support_only = FALSE, ...) {
  plot_data <- attr(x, "plot_data")
  x$ind <- x$Parameter

  # if we have intercept-only models, keep at least the intercept
  intercepts_data <- which(.in_intercepts(plot_data$ind))
  if (length(intercepts_data) &&
    nrow(plot_data) > length(intercepts_data) &&
    !show_intercept) {
    intercepts_si <- which(.in_intercepts(x$ind))
    x <- x[-intercepts_si, ]
    plot_data <- plot_data[-intercepts_data, ]
  }


  if (length(unique(x$CI)) > 1) {
    p <- ggplot(mapping = aes(
      x = .data$x,
    )) +
      # SI
      geom_rect(
        aes(xmin = .data$CI_low, xmax = .data$CI_high, alpha = .data$CI),
        ymin = 0, ymax = Inf,
        data = x,
        fill = si_color,
        inherit.aes = FALSE
      ) +
      scale_alpha_continuous(breaks = unique(x$CI)) +
      labs(
        x = "",
        title = "Support Interval",
        alpha = "BF level"
      ) +
      theme(legend.position = "bottom")
  } else {
    # Basic plot
    p <- ggplot(mapping = aes(
      x = .data$x,
    )) +
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
        x = "",
        title = paste0("Support Interval (BF = ", x$CI[1], " SI)")
      ) +
      theme(legend.position = "bottom")
  }


  if (isTRUE(support_only)) {
    support_data <- split(plot_data, as.character(plot_data$ind))

    precision <- 2^8

    support_data <- lapply(support_data, function(d) {
      x_vals <- seq(min(d$x), max(d$x), length.out = precision)

      prior_y <- stats::approx(
        d$x[d$Distribution == "prior"],
        d$y[d$Distribution == "prior"],
        xout = x_vals
      )$y

      posterior_y <- stats::approx(
        d$x[d$Distribution == "posterior"],
        d$y[d$Distribution == "posterior"],
        xout = x_vals
      )$y

      updating_factor <- posterior_y / prior_y
      # updating_factor[is.na(updating_factor)] <- 0
      x_vals <- x_vals[!is.na(updating_factor)]
      updating_factor <- updating_factor[!is.na(updating_factor)]

      data.frame(
        updating_factor,
        x = x_vals,
        ind = d$ind[1]
      )
    })
    support_data <- do.call(rbind, support_data)

    p <- p +
      aes(y = .data$updating_factor) +
      # distributions
      geom_line(linewidth = 1, data = support_data) +
      geom_area(alpha = 0.15, data = support_data) +
      geom_hline(yintercept = unique(x$CI), colour = "grey30", linetype = "dotted") +
      labs(y = "Updating Factor")
  } else {
    p <- p +
      aes(
        y = .data$y,
        color = .data$Distribution,
        fill = .data$Distribution
      ) +
      # distributions
      geom_line(linewidth = 1, data = plot_data) +
      geom_area(alpha = 0.15, data = plot_data) +
      labs(y = "Density")
  }

  if (length(unique(plot_data$ind)) > 1L) {
    p <- p + facet_wrap(~ind, scales = "free")
  }

  p
}
