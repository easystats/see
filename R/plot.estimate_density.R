#' @export
data_plot.estimate_density <- function(x,
                                       data = NULL,
                                       centrality = "median",
                                       ci = 0.95,
                                       ...) {
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

  # summary
  split_columns <- intersect(c("Parameter", "Effects", "Component"), colnames(dataplot))
  datasplit <- split(dataplot, dataplot[split_columns])
  summary <- do.call(rbind, insight::compact_list(lapply(datasplit, function(i) {
    if (length(i$x) > 0L) {
      Estimate <- as.numeric(bayestestR::point_estimate(i$x, centrality = centrality))
      CI <- as.numeric(bayestestR::ci(i$x, ci = ci))
      out <- data.frame(
        Parameter = unique(i$Parameter),
        x = Estimate,
        CI_low = CI[2],
        CI_high = CI[3],
        stringsAsFactors = FALSE
      )
      if ("Effects" %in% colnames(i)) {
        out$Effects <- unique(i$Effects)
      }
      if ("Component" %in% colnames(i)) {
        out$Component <- unique(i$Component)
      }
    } else {
      out <- NULL
    }
    out
  })))

  summary$Parameter <- factor(summary$Parameter)
  summary$Parameter <- factor(summary$Parameter, levels = levels(dataplot$Parameter))

  attr(dataplot, "summary") <- summary
  attr(dataplot, "info") <- list(
    "xlab" = "Values",
    "ylab" = "Density",
    "legend_fill" = "Parameter",
    "legend_color" = "Parameter",
    "title" = "Estimated Density Function"
  )

  class(dataplot) <- c("data_plot", "see_estimate_density", class(dataplot))
  dataplot
}


# Plot --------------------------------------------------------------------

#' Plot method for density estimation of posterior samples
#'
#' The `plot()` method for the `bayestestR::estimate_density()` function.
#'
#' @param stack Logical. If `TRUE`, densities are plotted as stacked lines.
#'   Else, densities are plotted for each parameter among each other.
#' @param priors Logical. If `TRUE`, prior distributions are simulated
#'   (using [bayestestR::simulate_prior()]) and added
#'   to the plot.
#' @param priors_alpha Numeric value specifying alpha for the prior
#'   distributions.
#' @param posteriors_alpha Numeric value specifying alpha for the posterior
#'   distributions.
#' @param centrality Character specifying the point-estimate (centrality index)
#'   to compute. Can be `"median"`, `"mean"` or `"MAP"`.
#' @param ci Numeric value of probability of the CI (between 0 and 1) to be
#'   estimated. Default to `0.95`.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_parameters_model
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")
#' library(rstanarm)
#' library(bayestestR)
#' set.seed(123)
#' m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
#' result <- estimate_density(m)
#' plot(result)
#' @importFrom ggplot2 .data
#' @export
plot.see_estimate_density <- function(x,
                                      stack = TRUE,
                                      show_intercept = FALSE,
                                      n_columns = 1,
                                      priors = FALSE,
                                      priors_alpha = 0.4,
                                      posteriors_alpha = 0.7,
                                      size_line = 0.9,
                                      size_point = 2,
                                      centrality = "median",
                                      ci = 0.95,
                                      ...) {
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


  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = model, centrality = centrality, ci = ci, ...)
  }

  if ((!"Effects" %in% names(x) || length(unique(x$Effects)) <= 1) &&
    (!"Component" %in% names(x) || length(unique(x$Component)) <= 1)) {
    n_columns <- NULL
  }

  # get labels
  labels <- .clean_parameter_names(x$Parameter, grid = !is.null(n_columns))

  # remove intercept from output, if requested
  x <- .remove_intercept(x, show_intercept = show_intercept)

  if (stack) {
    p <- ggplot(x, aes(x = .data$x, y = .data$y, color = .data$Parameter)) +
      geom_line(linewidth = size_line) +
      add_plot_attributes(x) +
      scale_color_flat(labels = labels)
  } else {
    p <- ggplot(x, aes(x = .data$x, y = .data$Parameter, height = .data$y))

    # add prior layer
    if (priors) {
      insight::check_if_installed("ggridges")

      p <- p +
        .add_prior_layer_ridgeline(
          model,
          show_intercept = show_intercept,
          priors_alpha = priors_alpha,
          show_ridge_line = FALSE
        ) +
        ggridges::geom_ridgeline(aes(fill = "Posterior"),
          alpha = posteriors_alpha,
          color = NA
        ) +
        guides(color = "none") +
        scale_fill_flat(reverse = TRUE) +
        scale_colour_flat(reverse = TRUE)
    } else {
      insight::check_if_installed("ggridges")

      p <- p +
        ggridges::geom_ridgeline(aes(fill = "Posterior"),
          alpha = posteriors_alpha,
          color = NA
        ) +
        guides(fill = "none", color = "none") +
        scale_fill_manual(values = unname(social_colors("grey"))) +
        scale_color_manual(values = unname(social_colors("grey")))
    }

    summary <- attributes(x)$summary
    summary <- .remove_intercept(summary, show_intercept = show_intercept)
    summary$y <- NA

    p <- p +
      geom_errorbarh(
        data = summary,
        mapping = aes(
          xmin = .data$CI_low,
          xmax = .data$CI_high,
          color = "Posterior"
        ),
        linewidth = size_line
      ) +
      geom_point(
        data = summary,
        mapping = aes(x = .data$x, color = "Posterior"),
        size = size_point,
        fill = "white",
        shape = 21
      )

    p <- p + add_plot_attributes(x)
  }


  if (length(unique(x$Parameter)) == 1 || isTRUE(stack)) {
    p <- p + scale_y_continuous(breaks = NULL, labels = NULL)
  } else {
    p <- p + scale_y_discrete(labels = labels)
  }


  if (length(unique(x$Parameter)) == 1) {
    p <- p + guides(color = "none")
  }


  if (!is.null(n_columns)) {
    if ("Component" %in% names(x) && "Effects" %in% names(x)) {
      p <- p + facet_wrap(~ Effects + Component, scales = "free", ncol = n_columns)
    } else if ("Effects" %in% names(x)) {
      p <- p + facet_wrap(~Effects, scales = "free", ncol = n_columns)
    } else if ("Component" %in% names(x)) {
      p <- p + facet_wrap(~Component, scales = "free", ncol = n_columns)
    }
  }

  p
}




# Density df --------------------------------------------------------------------

#' @export
data_plot.estimate_density_df <- data_plot.estimate_density


#' @importFrom ggplot2 .data
#' @export
plot.see_estimate_density_df <- function(x,
                                         stack = TRUE,
                                         n_columns = 1,
                                         size_line = 0.9,
                                         ...) {
  x$Parameter <- factor(x$Parameter, levels = rev(unique(x$Parameter)))
  labels <- stats::setNames(levels(x$Parameter), levels(x$Parameter))

  if (stack) {
    p <- ggplot(x, aes(x = .data$x, y = .data$y, color = .data$Parameter)) +
      geom_line(linewidth = size_line)
  } else {
    insight::check_if_installed("ggridges")

    p <- ggplot(x, aes(x = .data$x, y = .data$Parameter, height = .data$y)) +
      ggridges::geom_ridgeline()
  }


  if (length(unique(x$Parameter)) == 1 || isTRUE(stack)) {
    p <- p + scale_y_continuous(breaks = NULL, labels = NULL)
  } else {
    p <- p + scale_y_discrete(labels = labels)
  }

  if (length(unique(x$Parameter)) == 1L) {
    p <- p + guides(color = "none")
  }

  if ("Group" %in% names(x)) {
    p <- p + facet_wrap(~Group, scales = "free", ncol = n_columns)
  }

  p
}
