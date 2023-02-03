#' Plot method for (non-)constant error variance checks
#'
#' The `plot()` method for the
#' `performance::check_heteroscedasticity()` function.
#'
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- check_heteroscedasticity(m)
#' result
#' plot(result, data = m) # data required for pkgdown
#' @export
plot.see_check_heteroscedasticity <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }

  faminfo <- insight::model_info(model)
  r <- tryCatch(
    {
      if (inherits(model, "merMod")) {
        stats::residuals(model, scaled = TRUE)
      } else if (inherits(model, c("glmmTMB", "MixMod"))) {
        sigma <- if (faminfo$is_mixed) {
          sqrt(insight::get_variance_residual(model))
        } else {
          .sigma_glmmTMB_nonmixed(model, faminfo)
        }
        stats::residuals(model) / sigma
      } else if (inherits(model, "glm")) {
        stats::rstandard(model, type = "pearson")
      } else {
        stats::rstandard(model)
      }
    },
    error = function(e) {
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
  .plot_diag_homogeneity(dat, size_point = 2, size_line = 0.8, ...)
}


.sigma_glmmTMB_nonmixed <- function(model, faminfo) {
  if (!is.na(match(faminfo$family, c("binomial", "poisson", "truncated_poisson")))) {
    return(1)
  }
  betad <- model$fit$par["betad"]

  switch(faminfo$family,
    gaussian = exp(0.5 * betad),
    Gamma = exp(-0.5 * betad),
    exp(betad)
  )
}
