#' @importFrom insight clean_parameters
#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.estimate_density <- function(x, data = NULL, ...) {
  dataplot <- x

  if (!"Parameter" %in% names(dataplot)) {
    dataplot$Parameter <- "Distribution"
  }

  # add component and effects columns
  if (!is.null(data)) {
    dataplot <- merge(dataplot, insight::clean_parameters(data), by = "Parameter")
  }

  dataplot <- .fix_facet_names(dataplot)

  dataplot$Parameter <- factor(dataplot$Parameter)
  dataplot$Parameter <- factor(dataplot$Parameter, levels = rev(levels(dataplot$Parameter)))


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
#' @importFrom rlang .data
#' @importFrom ggridges geom_ridgeline
#' @export
plot.see_estimate_density <- function(x, stack = TRUE, show_intercept = FALSE, n_columns = 1, priors = FALSE, priors_alpha = .4, size = .9, ...) {
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
    x <- data_plot(x, data = model, ...)
  }

  if ((!"Effects" %in% names(x) || length(unique(x$Effects)) <= 1) &&
      (!"Component" %in% names(x) || length(unique(x$Component)) <= 1)) n_columns <- NULL

  # get labels
  labels <- .clean_parameter_names(x$Parameter, grid = !is.null(n_columns))

  # remove intercept from output, if requested
  x <- .remove_intercept(x, show_intercept = show_intercept)

  if (stack == TRUE) {
    p <- x %>%
      ggplot(aes(
        x = .data$x,
        y = .data$y,
        color = .data$Parameter
      )) +
      geom_line(size = size) +
      add_plot_attributes(x) +
      scale_color_flat(labels = labels)
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


  if (length(unique(x$Parameter)) == 1 || isTRUE(stack)) {
    p <- p + scale_y_continuous(breaks = NULL, labels = NULL)
  } else {
    p <- p + scale_y_discrete(labels = labels)
  }


  if (!is.null(n_columns)) {
    if ("Component" %in% names(x) && "Effects" %in% names(x)) {
      p <- p + facet_wrap(~ Effects + Component, scales = "free", ncol = n_columns)
    } else if ("Effects" %in% names(x)) {
      p <- p + facet_wrap(~ Effects, scales = "free", ncol = n_columns)
    } else if ("Component" %in% names(x)) {
      p <- p + facet_wrap(~ Component, scales = "free", ncol = n_columns)
    }
  }

  p
}

