#' @export
data_plot.estimate_density <- function(
  x,
  data = NULL,
  centrality = "median",
  ci = 0.95,
  ...
) {
  dataplot <- x

  if (!"Parameter" %in% names(dataplot)) {
    dataplot$Parameter <- "Distribution"
  }

  # add component and effects columns
  if (!is.null(data)) {
    dataplot <- merge(
      dataplot,
      insight::clean_parameters(data),
      by = "Parameter"
    )
  }

  dataplot <- .fix_facet_names(dataplot)

  dataplot$Parameter <- factor(dataplot$Parameter)
  dataplot$Parameter <- factor(
    dataplot$Parameter,
    levels = rev(levels(dataplot$Parameter))
  )

  # summary
  split_columns <- intersect(
    c("Parameter", "Effects", "Component"),
    colnames(dataplot)
  )
  datasplit <- split(dataplot, dataplot[split_columns])
  my_summary <- do.call(
    rbind,
    insight::compact_list(lapply(datasplit, function(i) {
      if (length(i$x) > 0L) {
        Estimate <- as.numeric(bayestestR::point_estimate(
          i$x,
          centrality = centrality
        ))
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
    }))
  )

  my_summary$Parameter <- factor(my_summary$Parameter)
  my_summary$Parameter <- factor(
    my_summary$Parameter,
    levels = levels(dataplot$Parameter)
  )

  attr(dataplot, "summary") <- my_summary
  attr(dataplot, "info") <- list(
    xlab = "Values",
    ylab = "Density",
    legend_fill = "Parameter",
    legend_color = "Parameter",
    title = "Estimated Density Function"
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
#' @param alpha_priors Numeric value specifying alpha for the prior
#'   distributions.
#' @param alpha_posteriors Numeric value specifying alpha for the posterior
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
#'
#' @export
plot.see_estimate_density <- function(
  x,
  stack = TRUE,
  show_intercept = FALSE,
  n_columns = 1,
  priors = FALSE,
  alpha_priors = 0.4,
  alpha_posteriors = 0.7,
  linewidth = 0.9,
  size_point = 2,
  centrality = "median",
  ci = 0.95,
  ...
) {
  # save model for later use
  model <- tryCatch(
    .retrieve_data(x),
    error = function(e) {
      priors <- FALSE
      NULL
    }
  )

  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = model, centrality = centrality, ci = ci, ...)
  }

  if (.has_multiple_panels(x)) {
    n_columns <- NULL
  }

  # get parameter names for filtering
  params <- unique(x$Parameter)

  # get labels
  parameter_labels <- .clean_parameter_names(
    x$Parameter,
    grid = !is.null(n_columns)
  )

  # remove intercept from output, if requested
  x <- .remove_intercept(x, show_intercept = show_intercept)

  if (stack) {
    p <- ggplot(x, aes(x = .data$x, y = .data$y, color = .data$Parameter)) +
      geom_line(linewidth = linewidth) +
      add_plot_attributes(x) +
      scale_color_flat(labels = parameter_labels)
  } else {
    p <- ggplot(x, aes(x = .data$x, y = .data$Parameter, height = .data$y))

    # add prior layer
    if (priors) {
      insight::check_if_installed("ggridges")

      p <- p +
        .add_prior_layer_ridgeline(
          model,
          parameter = params,
          show_intercept = show_intercept,
          alpha_priors = alpha_priors,
          show_ridge_line = FALSE
        ) +
        ggridges::geom_ridgeline(
          aes(fill = "Posterior"),
          alpha = alpha_posteriors,
          color = NA
        ) +
        guides(color = "none") +
        scale_fill_flat(reverse = TRUE) +
        scale_colour_flat(reverse = TRUE)
    } else {
      insight::check_if_installed("ggridges")

      p <- p +
        ggridges::geom_ridgeline(
          aes(fill = "Posterior"),
          alpha = alpha_posteriors,
          color = NA
        ) +
        guides(fill = "none", color = "none") +
        scale_fill_manual(values = unname(social_colors("grey"))) +
        scale_color_manual(values = unname(social_colors("grey")))
    }

    my_summary <- attributes(x)$summary
    my_summary <- .remove_intercept(my_summary, show_intercept = show_intercept)
    my_summary$y <- NA

    p <- p +
      geom_errorbar(
        data = my_summary,
        mapping = aes(
          xmin = .data$CI_low,
          xmax = .data$CI_high,
          color = "Posterior"
        ),
        orientation = "y",
        linewidth = linewidth
      ) +
      geom_point(
        data = my_summary,
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
    p <- p + scale_y_discrete(labels = parameter_labels)
  }

  if (length(unique(x$Parameter)) == 1) {
    p <- p + guides(color = "none")
  }

  if (!is.null(n_columns)) {
    if ("Component" %in% names(x) && "Effects" %in% names(x)) {
      p <- p +
        facet_wrap(~ Effects + Component, scales = "free", ncol = n_columns)
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


#'
#' @export
plot.see_estimate_density_df <- function(
  x,
  stack = TRUE,
  n_columns = 1,
  linewidth = 0.9,
  ...
) {
  x$Parameter <- factor(x$Parameter, levels = rev(unique(x$Parameter)))
  parameter_labels <- stats::setNames(levels(x$Parameter), levels(x$Parameter))

  if (stack) {
    p <- ggplot(x, aes(x = .data$x, y = .data$y, color = .data$Parameter)) +
      geom_line(linewidth = linewidth)
  } else {
    insight::check_if_installed("ggridges")

    p <- ggplot(x, aes(x = .data$x, y = .data$Parameter, height = .data$y)) +
      ggridges::geom_ridgeline()
  }

  if (length(unique(x$Parameter)) == 1 || isTRUE(stack)) {
    p <- p + scale_y_continuous(breaks = NULL, labels = NULL)
  } else {
    p <- p + scale_y_discrete(labels = parameter_labels)
  }

  if (length(unique(x$Parameter)) == 1L) {
    p <- p + guides(color = "none")
  }

  if ("Group" %in% names(x)) {
    p <- p + facet_wrap(~Group, scales = "free", ncol = n_columns)
  }

  p
}
