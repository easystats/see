#' @importFrom parameters model_simulate
#' @importFrom bayestestR estimate_density
#' @export
data_plot.parameters_simulate <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  n_sims <- attr(x, "n_sims")
  dat <- parameters::model_simulate(model = data, n_sims = n_sims)
  params <- insight::clean_parameters(data)

  out <- bayestestR::estimate_density(dat)

  if (length(unique(params$Effects)) > 1) {
    out$Effects <- NA

    if (length(unique(params$Component)) > 1) {
      zi_comp <- params$Component == "zero_inflated"
      params$Parameter[zi_comp] <- paste0(params$Parameter[zi_comp], "_zi")
    }

    for (i in names(dat)) {
      if (i %in% params$Parameter && i %in% out$Parameter) {
        out$Effects[out$Parameter == i] <- params$Effects[params$Parameter == i]
      }
    }

    if (length(unique(params$Component)) > 1) {
      out$Component <- NA
      for (i in names(dat)) {
        if (i %in% params$Parameter && i %in% out$Parameter) {
          out$Component[out$Parameter == i] <- params$Component[params$Parameter == i]
        }
      }
    }
  }

  out
}


# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @export
plot.see_parameters_simulate <- function(x, data = NULL, stack = TRUE, show_intercept = FALSE, n_columns = NULL, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  plot.see_estimate_density(x, stack = stack, show_intercept = show_intercept, n_columns = n_columns, ...)
}