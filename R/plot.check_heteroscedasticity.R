#' Plot method for (non-)constant error variance checks
#'
#' The \code{plot()} method for the
#' \code{performance::check_heteroscedasticity()} function.
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
#' @importFrom stats residuals sd dnorm
#' @export
plot.see_check_heteroscedasticity <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }
  dat <- data.frame(
    x = stats::fitted(model),
    y = stats::residuals(model)
  )
  .plot_diag_ncv(dat, size_point = 2, size_line = .8)
}
