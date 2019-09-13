#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.estimate_density <- function(x, ...) {
  dataplot <- x

  if (!"Parameter" %in% names(dataplot)) {
    dataplot$Parameter <- "Distribution"
  }

  attr(dataplot, "info") <- list("xlab" = "Values",
                                 "ylab" = "Density",
                                 "legend_fill" = "Parameter",
                                 "legend_color" = "Parameter",
                                 "title" = "Estimated Density Function")

  class(dataplot) <- c("data_plot", "see_estimate_density", class(dataplot))
  dataplot
}





# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @param stack Logical, if \code{TRUE}, densities are plotted as stacked lines.
#'   Else, densities are plotted for each parameter among each other.
#' @inheritParams plot.see_point_estimate
#' @importFrom rlang .data
#' @importFrom ggridges geom_ridgeline
#' @export
plot.see_estimate_density <- function(x, stack = TRUE, show_intercept = FALSE, grid = FALSE, priors = FALSE, priors_alpha = .4, ...) {
  # save model for later use
  model <- tryCatch(
    {
      .retrieve_data(x)
    },
    error = function(e) {
      priors <- FALSE
      NULL
    }
  )


  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, ...)
  }

  # remove intercept from output, if requested
  x <- .remove_intercept(x, show_intercept = show_intercept)

  if (stack == TRUE) {
    p <- x %>%
      ggplot(aes(
        x = .data$x,
        y = .data$y,
        color = .data$Parameter
      )) +
      geom_line() +
      add_plot_attributes(x)
  } else {
    p <- x %>%
      ggplot(aes(
        x = .data$x,
        y = .data$Parameter,
        height = .data$y
      ))

    # add prior layer
    if (priors) {
      p <- p +
        .add_prior_layer_ridgeline(
          model,
          show_intercept = show_intercept,
          priors_alpha = priors_alpha
        ) +
        ggridges::geom_ridgeline(aes(fill = "Posterior"), alpha = .7)
    } else {
      p <- p + ggridges::geom_ridgeline()
    }

    p <- p + add_plot_attributes(x)
  }

  if ("Effects" %in% names(x) && isTRUE(grid)) {
    if ("Component" %in% names(x))
      p <- p + facet_wrap(~ Effects + Component, scales = "free")
    else
      p <- p + facet_wrap(~ Effects, scales = "free")
  }

  p
}

