#' @export
data_plot.point_estimate <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  if (inherits(data, "emmGrid")) {
    insight::check_if_installed("emmeans")

    data <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(data, names = FALSE)))
  } else if (inherits(data, c("stanreg", "brmsfit"))) {
    data <- insight::get_parameters(data, effects = "all", component = "all")
  } else if (inherits(data, "BFBayesFactor")) {
    data <- insight::get_parameters(data)
  } else if (inherits(data, "MCMCglmm")) {
    nF <- data$Fixed$nfl
    data <- as.data.frame(data$Sol[, 1:nF, drop = FALSE])
  } else {
    data <- as.data.frame(data)
  }

  data <- tryCatch(
    if (!is.null(x$Parameter)) {
      data[, x$Parameter, drop = FALSE]
    } else {
      data
    },
    error = function(e) {
      data
    }
  )

  centrality <- tolower(attr(x, "centrality", exact = TRUE))
  if (is.null(centrality)) centrality <- "all"

  dataplot <- lapply(colnames(data), function(i) {
    my_dist <- data[[i]]

    pe <- bayestestR::point_estimate(my_dist, centrality = "all")
    dat <- as.data.frame(stats::density(my_dist, n = 256))
    dat$group <- i

    if (any(centrality %in% c("all", "mean"))) {
      attr(dat, "mean_x") <- pe$Mean
      attr(dat, "mean_y") <- dat$y[which.min(abs(dat$x - pe$Mean))]
    }

    if (any(centrality %in% c("all", "median"))) {
      attr(dat, "median_x") <- pe$Median
      attr(dat, "median_y") <- dat$y[which.min(abs(dat$x - pe$Median))]
    }

    if (any(centrality %in% c("all", "map"))) {
      attr(dat, "map_x") <- pe$MAP
      attr(dat, "map_y") <- dat$y[which.min(abs(dat$x - pe$MAP))]
    }

    dat
  })

  class(dataplot) <- c("data_plot", "see_point_estimate", class(dataplot))
  names(dataplot) <- colnames(data)

  dataplot
}


#' @export
data_plot.map_estimate <- data_plot.point_estimate


# Plot --------------------------------------------------------------------

#' Plot method for point estimates of posterior samples
#'
#' The `plot()` method for the `bayestestR::point_estimate()`.
#'
#' @param show_labels Logical. If `TRUE`, the text labels for the point
#'   estimates (i.e. *"Mean"*, *"Median"* and/or *"MAP"*) are
#'   shown. You may set `show_labels = FALSE` in case of overlapping
#'   labels, and add your own legend or footnote to the plot.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_check_outliers
#' @inheritParams plot.see_check_distribution
#' @inheritParams plot.see_estimate_density
#'
#' @return A ggplot2-object.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")
#' library(rstanarm)
#' library(bayestestR)
#' set.seed(123)
#' m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
#' result <- point_estimate(m, centrality = "median")
#' result
#' plot(result)
#'
#' @export
plot.see_point_estimate <- function(x,
                                    data = NULL,
                                    size_point = 2,
                                    size_text = 3.5,
                                    panel = TRUE,
                                    show_labels = TRUE,
                                    show_intercept = FALSE,
                                    priors = FALSE,
                                    alpha_priors = 0.4,
                                    ...) {
  # save model for later use
  model <- .retrieve_data(x)

  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = data)
  }

  p <- lapply(x, function(i) {
    mean_x <- attr(i, "mean_x")
    mean_y <- attr(i, "mean_y")
    median_x <- attr(i, "median_x")
    median_y <- attr(i, "median_y")
    map_x <- attr(i, "map_x")
    map_y <- attr(i, "map_y")
    max_y <- max(i$y)

    if ("group" %in% colnames(i)) {
      x_lab <- .clean_parameter_names(unique(i$group))
    } else {
      x_lab <- "Parameter Value"
    }

    if (!show_intercept && .is_intercept(x_lab)) {
      return(NULL)
    }

    label_mean_x <- mean_x
    label_mean_y <- max_y * 1.05
    label_median_x <- median_x
    label_median_y <- max_y * 1.05
    label_map_x <- map_x
    label_map_y <- max_y * 1.05

    p_object <- ggplot(i, aes(x = .data$x, y = .data$y, group = .data$group))

    # add prior layer
    if (priors) {
      p_object <- p_object + .add_prior_layer_ribbon(
        model,
        parameter = x_lab,
        show_intercept = show_intercept,
        alpha_priors = alpha_priors,
        fill_color = "#FF9800"
      )
      alpha_posteriors <- 0.7
    } else {
      alpha_posteriors <- 1
    }

    p_object <- p_object +
      geom_ribbon(aes(ymin = 0, ymax = .data$y),
        fill = "#FFC107",
        alpha = alpha_posteriors
      )

    if (!is.null(mean_x) && !is.null(mean_y)) {
      p_object <- p_object +
        geom_segment(
          x = mean_x,
          xend = mean_x,
          y = 0,
          yend = mean_y,
          color = "#E91E63",
          linewidth = 1,
          alpha = alpha_posteriors
        ) +
        geom_point(
          x = mean_x,
          y = mean_y,
          color = "#E91E63",
          size = size_point,
          alpha = alpha_posteriors
        )
      if (show_labels) {
        p_object <- p_object +
          geom_text(
            x = label_mean_x,
            y = label_mean_y,
            label = "Mean",
            color = "#E91E63",
            size = size_text
          )
      }
    }

    if (!is.null(median_x) && !is.null(median_y)) {
      p_object <- p_object +
        geom_segment(
          x = median_x,
          xend = median_x,
          y = 0,
          yend = median_y,
          color = "#2196F3",
          linewidth = 1,
          alpha = alpha_posteriors
        ) +
        geom_point(
          x = median_x,
          y = median_y,
          color = "#2196F3",
          size = size_point,
          alpha = alpha_posteriors
        )
      if (show_labels) {
        p_object <- p_object +
          geom_text(
            x = label_median_x,
            y = label_median_y,
            label = "Median",
            color = "#2196F3",
            size = size_text
          )
      }
    }

    if (!is.null(map_x) && !is.null(map_y)) {
      p_object <- p_object +
        geom_segment(
          x = map_x,
          xend = map_x,
          y = 0,
          yend = map_y,
          color = "#4CAF50",
          linewidth = 1,
          alpha = alpha_posteriors
        ) +
        geom_point(
          x = map_x,
          y = map_y,
          color = "#4CAF50",
          size = size_point,
          alpha = alpha_posteriors
        )
      if (show_labels) {
        p_object <- p_object +
          geom_text(
            x = label_map_x,
            y = label_map_y,
            label = "MAP",
            color = "#4CAF50",
            size = size_text
          )
      }
    }

    p_object <- p_object +
      geom_vline(xintercept = 0, linetype = "dotted") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max_y * 1.15)) +
      labs(title = "Bayesian Point Estimates", x = x_lab, y = "Probability Density")

    p_object
  })

  p <- insight::compact_list(p)

  if (length(x) == 1) {
    p[[1]]
  } else if (panel) {
    p <- lapply(p, function(i) i + labs(title = NULL, y = NULL))
    do.call(plots, p)
  } else {
    p
  }
}
