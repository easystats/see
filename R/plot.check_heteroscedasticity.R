#' @importFrom bayestestR estimate_density
#' @importFrom stats residuals sd dnorm
#' @export
plot.see_check_heteroscedasticity <- function(x, data = NULL) {
  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }
  dat <- data.frame(
    x = stats::fitted(model),
    y = stats::residuals(model)
  )
  .plot_diag_ncv(dat, dot_size = 2, line_size = .8)
}

