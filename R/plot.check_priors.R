#' @export
data_plot.performance_check_priors <- function(x, predictor = NULL, ...) {
  result <- modelbased::estimate_relation(
    x$model,
    by = predictor,
    keep_iterations = TRUE,
    verbose = FALSE
  )
  # reshape iterations for plotting. These are by default in wide format, but
  # for ggplot, we need them in long format.
  dataplot <- bayestestR::reshape_iterations(result)
  colnames(dataplot)[1] <- "predictor"

  class(dataplot) <- unique(c(
    "data_plot",
    "see_check_priors",
    class(dataplot)
  ))

  dataplot
}


#' Plot method for prior predictive checks
#'
#' The `plot()` method for the `performance::check_priors()` function.
#'
#' @param size_boxplot Numeric value specifying size of boxplot geoms.
#' @param alpha_boxplot Numeric value specifying alpha of boxplot geoms.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_bayesfactor_parameters
#'
#' @return A ggplot2-object.
#'
#' @examples
#' \dontrun{
#' library(performance)
#' # model with correctly defined priors. outcome is binary, prior
#' # predictive checks indicate the predicted probability mass based
#' # on the prior distributions - the resulting pattern aligns with
#' # our real-world assumptions
#' model <- insight::download_model("stan_prior_checks_1")
#' plot(performance::check_priors(model, "mmse"))
#'
#' # model with default (weakly informative) priors, which is poorly
#' # calibrated. It pushes probability mass almost exclusively to the
#' # extremes of 0 and 1, leaving the plausible middle range largely
#' # unsupported
#' model <- insight::download_model("stan_prior_checks_2")
#' plot(performance::check_priors(model, "mmse"))
#' }
#'
#' @export
plot.see_check_priors <- function(
  x,
  size_point = 2,
  size_boxplot = 0.4,
  size_title = 12,
  size_axis_title = base_size,
  base_size = 10,
  alpha_dot = 0.15,
  alpha_boxplot = 0.35,
  theme = NULL,
  colors = NULL,
  ...
) {
  theme <- .set_default_theme(
    x,
    theme,
    base_size,
    size_axis_title,
    size_title,
    default_theme = theme_modern()
  )
  # determine how many predictors to plot
  predictors <- attributes(x)$predictors

  if (is.null(predictors)) {
    insight::format_error(
      "No predictor was specified. Cannot plot prior predictive checks."
    )
  }

  out <- lapply(predictors, function(p) {
    pdata <- data_plot(x, predictor = p)

    ggplot2::ggplot(
      pdata,
      ggplot2::aes(
        x = .data$predictor,
        group = .data$iter_group,
        y = .data$iter_value,
        color = .data$predictor,
        fill = .data$predictor
      )
    ) +
      geom_jitter2(alpha = alpha_dot, size = size_point) +
      ggplot2::geom_boxplot(
        ggplot2::aes(group = NULL),
        alpha = alpha_boxplot,
        fill = "white",
        outliers = FALSE,
        width = size_boxplot
      ) +
      theme +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = NULL, y = NULL)
  })

  if (length(predictors) == 1) {
    out[[1]] + ggplot2::ggtitle(predictors[1])
  } else {
    plots(out, tags = predictors)
  }
}
