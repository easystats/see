#' @export
data_plot.parameters_simulate <- function(x,
                                          data = NULL,
                                          normalize_height = FALSE,
                                          ...) {
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  n_sims <- attr(x, "n_sims")
  dat <- parameters::simulate_model(model = data, n_sims = n_sims)
  params <- insight::clean_parameters(data)

  out <- bayestestR::estimate_density(dat)

  # normalize height
  if (isTRUE(normalize_height)) {
    out$y <- datawizard::rescale(out$y, to = c(0, 0.9))
  }

  if (length(unique(params$Effects)) > 1) {
    out$Effects <- NA

    if (length(unique(params$Component)) > 1) {
      zi_comp <- params$Component == "zero_inflated"
      params$Parameter[zi_comp] <- paste0(params$Parameter[zi_comp], "_zi")
      disp_comp <- params$Component == "dispersion"
      params$Parameter[disp_comp] <- paste0(params$Parameter[disp_comp], "_disp")
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

  if (!is.null(attributes(x)$object_class) && "mlm" %in% attributes(x)$object_class) {
    out$Component <- NA
    for (i in unique(params$Response)) {
      out$Component[grepl(paste0(i, "$"), out$Parameter)] <- i
      out$Parameter <- gsub(paste0(i, "$"), "", out$Parameter)
    }
  }

  out
}


# Plot --------------------------------------------------------------------

#' Plot method for simulated model parameters
#'
#' The `plot()` method for the `parameters::simulate_parameters()`
#' function.
#'
#' @param normalize_height Logical. If `TRUE`, height of density-areas is
#'   "normalized", to avoid overlap. In certain cases when the range of a
#'   distribution of simulated draws is narrow for some parameters, this may
#'   result in very flat density-areas. In such cases, set
#'   `normalize_height = FALSE`.
#' @inheritParams data_plot
#' @inheritParams plot.see_estimate_density
#' @inheritParams plot.see_bayesfactor_parameters
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(parameters)
#' m <<- lm(mpg ~ wt + cyl + gear, data = mtcars)
#' result <- simulate_parameters(m)
#' result
#' plot(result)
#' @export
plot.see_parameters_simulate <- function(x,
                                         data = NULL,
                                         stack = TRUE,
                                         show_intercept = FALSE,
                                         n_columns = NULL,
                                         normalize_height = FALSE,
                                         size_line = 0.9,
                                         posteriors_alpha = 0.7,
                                         centrality = "median",
                                         ci = 0.95,
                                         ...) {
  is_mlm <- !is.null(attributes(x)$object_class) && "mlm" %in% attributes(x)$object_class
  if (is.null(n_columns) && isTRUE(is_mlm)) n_columns <- 1

  # check for defaults
  if (missing(centrality) && !is.null(attributes(x)$centrality)) {
    centrality <- attributes(x)$centrality
  }

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data, normalize_height = normalize_height)
  }

  plot.see_estimate_density(
    x,
    stack = stack,
    show_intercept = show_intercept,
    n_columns = n_columns,
    size_line = size_line,
    posteriors_alpha = posteriors_alpha,
    centrality = centrality,
    ci = ci,
    ...
  )
}
