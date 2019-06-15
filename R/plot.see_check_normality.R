#' @importFrom bayestestR estimate_density
#' @importFrom stats residuals sd dnorm
#' @rdname data_plot
#' @export
plot.see_check_normality <- function(x) {

  model <- .retrieve_data(x)
  r <- stats::residuals(model)
  dat <- as.data.frame(bayestestR::estimate_density(r))
  dat$curve <- stats::dnorm(seq(min(dat$x), max(dat$x), length.out = nrow(dat)),  mean(r),  stats::sd(r))

  .plot_diag_norm(dat, line_size = .8)
}

