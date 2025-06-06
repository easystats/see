#' Plot method for (non-)constant error variance checks
#'
#' The `plot()` method for the
#' `performance::check_heteroscedasticity()` function.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @seealso See also the vignette about [`check_model()`](https://easystats.github.io/performance/articles/check_model.html).
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- performance::check_heteroscedasticity(m)
#' result
#' plot(result, data = m) # data required for pkgdown
#' @export
plot.see_check_heteroscedasticity <- function(
  x,
  data = NULL,
  size_point = 2,
  linewidth = 0.8,
  size_title = 12,
  size_axis_title = base_size,
  base_size = 10,
  ...
) {
  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }

  faminfo <- insight::model_info(model)
  r <- tryCatch(
    if (inherits(model, "merMod")) {
      stats::residuals(model, scaled = TRUE)
    } else if (inherits(model, c("glmmTMB", "MixMod"))) {
      sig <- if (faminfo$is_mixed) {
        sqrt(insight::get_variance_residual(model))
      } else {
        .sigma_glmmTMB_nonmixed(model, faminfo)
      }
      stats::residuals(model) / sig
    } else if (inherits(model, "glm")) {
      stats::rstandard(model, type = "pearson")
    } else {
      stats::rstandard(model)
    },
    error = function(e) {
      # debugging
      if (isTRUE(getOption("easystats_errors", FALSE))) {
        insight::format_error(e$message)
      }
      NULL
    }
  )

  if (is.null(r)) {
    r <- tryCatch(
      as.vector(scale(stats::residuals(model))),
      error = function(e) NULL
    )
  }

  if (is.null(r)) {
    insight::format_warning(
      sprintf(
        "Homogeneity of variance could not be computed. Cannot extract residual variance from objects of class '%s'.",
        class(model)[1]
      )
    )
    return(NULL)
  }

  dat <- data.frame(
    x = stats::fitted(model),
    y = sqrt(abs(r))
  )
  .plot_diag_homogeneity(
    dat,
    size_point = size_point,
    linewidth = linewidth,
    base_size = base_size,
    size_title = size_title,
    size_axis_title = size_axis_title,
    ...
  )
}


.sigma_glmmTMB_nonmixed <- function(model, faminfo) {
  if (!is.na(match(faminfo$family, c("binomial", "poisson", "truncated_poisson")))) {
    return(1)
  }
  betad <- model$fit$par["betad"]

  switch(
    faminfo$family,
    gaussian = exp(0.5 * betad),
    Gamma = exp(-0.5 * betad),
    exp(betad)
  )
}
