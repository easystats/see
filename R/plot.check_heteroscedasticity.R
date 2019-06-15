#' @importFrom bayestestR estimate_density
#' @importFrom stats residuals sd dnorm
#' @rdname data_plot
#' @export
plot.see_check_heteroscedasticity <- function(x) {
  model <- .retrieve_data(x)
  dat <- data.frame(
    x = stats::fitted(model),
    y = stats::residuals(model)
  )
  .plot_diag_ncv(dat, dot_size = 2, line_size = .8)
}

