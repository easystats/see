#' Plot method for check model for (non-)normality of residuals
#'
#' The \code{plot()} method for the \code{performance::check_normality()} function.
#'
#' @param type Character vector, indicating the type of plot.
#' @param size_line Size of line geoms.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- check_normality(m)
#' plot(result)
#' @importFrom bayestestR estimate_density
#' @importFrom stats residuals sd dnorm rstudent ppoints pnorm fitted
#' @export
plot.see_check_normality <- function(x, type = c("density", "qq", "pp"), data = NULL, size_line = .8, size_point = 2, ...) {
  type <- match.arg(type)

  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }

  # check type
  if (!is.null(attributes(x)$effects) && attributes(x)$effects == "random") {
    .plot_diag_reqq(attributes(x)$re_qq, size_point = size_point, size_line = size_line)
  } else {
    if (type == "qq") {
      if (inherits(model, c("lme", "lmerMod", "merMod", "glmmTMB"))) {
        res_ <- sort(stats::residuals(model), na.last = NA)
      } else {
        res_ <- sort(stats::rstudent(model), na.last = NA)
      }

      dat <- stats::na.omit(data.frame(y = res_))
      .plot_diag_qq(dat, size_point = size_point, size_line = size_line)
    } else if (type == "density") {
      r <- stats::residuals(model)
      dat <- as.data.frame(bayestestR::estimate_density(r))
      dat$curve <- stats::dnorm(
        seq(min(dat$x), max(dat$x), length.out = nrow(dat)),
        mean(r),
        stats::sd(r)
      )
      .plot_diag_norm(dat, size_line = size_line)
    } else if (type == "pp") {
      x <- sort(stats::residuals(model), na.last = NA)
      dat <- data.frame(res = x)
      .plot_diag_pp(dat, size_point = size_point, size_line = size_line)
    }
  }
}

