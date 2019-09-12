#' @importFrom parameters model_simulate
#' @importFrom bayestestR estimate_density
#' @export
data_plot.parameters_simulate <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  n_sims <- attr(x, "n_sims")
  dat <- parameters::model_simulate(model = data, n_sims = n_sims)
  bayestestR::estimate_density(dat)
}


# Plot --------------------------------------------------------------------
#' @export
plot.see_parameters_simulate <- function(x, data = NULL, stack = TRUE, show_intercept = FALSE, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  plot.see_estimate_density(x, stack = stack, show_intercept = show_intercept, ...)
}