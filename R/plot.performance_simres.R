#' Plot method for check model for (non-)normality of residuals
#'
#' The `plot()` method for the `performance::check_residuals()` resp.
#' `performance::simulate_residuals()` function.
#'
#' @param transform Function to transform the residuals. If `NULL` (default),
#' no transformation is applied and uniformly distributed residuals are expected.
#' See argument `quantileFuntion` in `?DHARMa:::residuals.DHARMa` for more details.
#'
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_check_model
#'
#' @return A ggplot2-object.
#'
#' @seealso See also the vignette about [`check_model()`](https://easystats.github.io/performance/articles/check_model.html).
#'
#' @examplesIf insight::check_if_installed("performance", "0.10.9.7") && require("glmmTMB") && require("qqplotr") && require("DHARMa")
#' data(Salamanders, package = "glmmTMB")
#' model <- glmmTMB::glmmTMB(
#'   count ~ mined + spp + (1 | site),
#'   family = poisson(),
#'   data = Salamanders
#' )
#' simulated_residuals <- performance::simulate_residuals(model)
#' plot(simulated_residuals)
#'
#' # or
#' simulated_residuals <- performance::simulate_residuals(model)
#' result <- performance::check_residuals(simulated_residuals)
#' plot(result)
#'
#' @export
plot.see_performance_simres <- function(x,
                                        size_line = 0.8,
                                        size_point = 1,
                                        alpha = 0.2,
                                        dot_alpha = 0.8,
                                        colors = c("#3aaf85", "#1b6ca8"),
                                        detrend = FALSE,
                                        transform = NULL,
                                        style = theme_lucid,
                                        ...) {
  dp <- list(min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)

  # need DHARMa to be installed
  insight::check_if_installed("DHARMa")

  # extract data, if from check_residuals
  if (inherits(x, "see_check_residuals")) {
    x <- attributes(x)$data
  }

  if (is.null(transform)) {
    res <- stats::residuals(x)
  } else {
    res <- stats::residuals(x, quantileFunction = transform)
  }

  # base plot information
  gg_init <- ggplot2::ggplot(
    data.frame(scaled_residuals = res),
    ggplot2::aes(sample = .data$scaled_residuals)
  )

  # when we have package qqplotr, we can add confidence bands
  if (requireNamespace("qqplotr", quietly = TRUE)) {
    qq_stuff <- list(
      qqplotr::stat_qq_band(
        distribution = "unif",
        dparams = list(min = 0, max = 1),
        alpha = alpha,
        detrend = detrend
      ),
      qqplotr::stat_qq_line(
        distribution = "unif",
        dparams = dp,
        size = size_line,
        colour = colors[1],
        detrend = detrend
      ),
      qqplotr::stat_qq_point(
        distribution = "unif",
        dparams = dp,
        size = size_point,
        alpha = dot_alpha,
        colour = colors[2],
        detrend = detrend
      )
    )
    if (detrend) {
      y_lab <- "Sample Quantile Deviations"
    } else {
      y_lab <- "Sample Quantiles"
    }
  } else {
    insight::format_alert("For confidence bands, please install `qqplotr`.")
    qq_stuff <- list(
      ggplot2::geom_qq(
        shape = 16,
        stroke = 0,
        distribution = stats::qunif,
        dparams = dp,
        size = size_point,
        colour = colors[2]
      ),
      ggplot2::geom_qq_line(
        linewidth = size_line,
        colour = colors[1],
        na.rm = TRUE,
        distribution = stats::qunif,
        dparams = dp
      )
    )
    y_lab <- "Sample Quantiles"
  }

  gg_init +
    qq_stuff +
    ggplot2::labs(
      title = "Uniformity of Residuals",
      subtitle = "Dots should fall along the line",
      x = "Standard Uniform Distribution Quantiles",
      y = y_lab
    ) +
    style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    )
}

#' @export
plot.see_check_residuals <- plot.see_performance_simres
