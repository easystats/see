#' Plot method to check model for overdispersion
#'
#' The `plot()` method for the `performance::check_overdispersion()`
#' function.
#'
#' @param size_line Numeric value specifying size of line geoms.
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' if (require("glmmTMB")) {
#'   data(Salamanders)
#'   m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#'   result <- check_overdispersion(m)
#'   plot(result)
#' }
#' @export
plot.see_check_overdisp <- function(x,
                                    size_line = .8,
                                    colors = c("#3aaf85", "#1b6ca8"),
                                    ...) {
  model <- .retrieve_data(x)
  d <- .diag_overdispersion(model)

  .plot_diag_overdispersion(
    d,
    style = theme_lucid,
    colors = colors,
    size_line = size_line
  )
}




.diag_overdispersion <- function(model) {
  faminfo <- insight::model_info(model)

  # data for poisson models
  if (faminfo$is_poisson && !faminfo$is_zero_inflated) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    d$V <- d$Predicted
  }

  # data for negative binomial models
  if (faminfo$is_negbin && !faminfo$is_zero_inflated) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    d$V <- d$Predicted * (1 + d$Predicted / insight::get_sigma(model))
  }

  # data for zero-inflated poisson models
  if (faminfo$is_poisson && faminfo$is_zero_inflated) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    if (inherits(model, "glmmTMB")) {
      ptype <- "zprob"
    } else {
      ptype <- "zero"
    }
    d$Prob <- stats::predict(model, type = ptype)
    d$V <- d$Predicted * (1 - d$Prob) * (1 + d$Predicted * d$Prob)
  }

  # data for zero-inflated negative binomial models
  if (faminfo$is_negbin && faminfo$is_zero_inflated && !faminfo$is_dispersion) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    if (inherits(model, "glmmTMB")) {
      ptype <- "zprob"
    } else {
      ptype <- "zero"
    }
    d$Prob <- stats::predict(model, type = ptype)
    d$Disp <- insight::get_sigma(model)
    d$V <- d$Predicted * (1 + d$Predicted / d$Disp) * (1 - d$Prob) * (1 + d$Predicted * (1 + d$Predicted / d$Disp) * d$Prob)
  }

  # data for zero-inflated negative binomial models with dispersion
  if (faminfo$is_negbin && faminfo$is_zero_inflated && faminfo$is_dispersion) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    if (inherits(model, "glmmTMB")) {
      ptype <- "zprob"
    } else {
      ptype <- "zero"
    }
    d$Prob <- stats::predict(model, type = ptype)
    d$Disp <- stats::predict(model, type = "disp")
    d$V <- d$Predicted * (1 + d$Predicted / d$Disp) * (1 - d$Prob) * (1 + d$Predicted * (1 + d$Predicted / d$Disp) * d$Prob)
  }

  d
}

