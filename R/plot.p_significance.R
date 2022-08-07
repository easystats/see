#' @export
data_plot.p_significance <- function(x,
                                     data = NULL,
                                     grid = TRUE,
                                     show_intercept = FALSE,
                                     ...) {
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  params <- NULL

  if (inherits(data, "emmGrid")) {
    insight::check_if_installed("emmeans")

    data <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(data, names = FALSE)))
  } else if (inherits(data, c("stanreg", "brmsfit"))) {
    params <- insight::clean_parameters(data)
    data <- as.data.frame(data, optional = FALSE)
  } else if (inherits(data, "BFBayesFactor")) {
    data <- insight::get_parameters(data)
  } else if (inherits(data, "MCMCglmm")) {
    params <- insight::clean_parameters(data)
    nF <- data$Fixed$nfl
    data <- as.data.frame(data$Sol[, 1:nF, drop = FALSE])
  } else {
    data <- as.data.frame(data)
  }

  if (ncol(data) > 1) {
    levels_order <- rev(x$Parameter)
    data <- data[, x$Parameter, drop = FALSE]
    dataplot <- data.frame()
    for (i in names(data)) {
      if (!is.null(params)) {
        dataplot <- rbind(
          dataplot,
          cbind(
            .compute_densities_ps(data[[i]], name = i, threshold = attr(x, "threshold")),
            "Effects" = params$Effects[params$Parameter == i],
            "Component" = params$Component[params$Parameter == i]
          )
        )
      } else {
        dataplot <- rbind(dataplot, .compute_densities_ps(data[[i]], name = i, threshold = attr(x, "threshold")))
      }
    }

    if ("Effects" %in% names(dataplot) && "Component" %in% names(dataplot)) {
      if (length(unique(dataplot$Effects)) == 1 && length(unique(dataplot$Component)) == 1) {
        dataplot$Effects <- NULL
        dataplot$Component <- NULL
      } else {
        if (is.factor(dataplot$Effects)) {
          dataplot$Effects <- factor(dataplot$Effects, levels = sort(levels(dataplot$Effects)))
        } else {
          dataplot$Effects <- factor(dataplot$Effects, levels = unique(dataplot$Effects))
        }
        if (is.factor(dataplot$Component)) {
          dataplot$Component <- factor(dataplot$Component, levels = sort(levels(dataplot$Component)))
        } else {
          dataplot$Component <- factor(dataplot$Component, levels = unique(dataplot$Component))
        }
      }
    }
  } else {
    levels_order <- NULL
    dataplot <- .compute_densities_pd(data[, 1], name = "Posterior")
  }

  dataplot <- do.call(
    rbind,
    by(
      dataplot,
      list(dataplot$y, dataplot$fill),
      function(df) {
        df$n <- nrow(df)
        return(df)
      }
    )
  )
  dataplot <- do.call(
    rbind,
    by(
      dataplot,
      dataplot$y,
      function(df) {
        df$prop <- df$n / nrow(df)
        return(df)
      }
    )
  )
  dataplot$fill2 <- with(dataplot, ifelse(prop >= .5, "Most probable", "Less probable"))
  dataplot <- dataplot[, which(!names(dataplot) %in% c("n", "prop"))]

  if (!is.null(levels_order)) {
    dataplot$y <- factor(dataplot$y, levels = levels_order)
  }

  groups <- unique(dataplot$y)
  if (!show_intercept) {
    dataplot <- .remove_intercept(dataplot, column = "y", show_intercept)
    groups <- unique(setdiff(groups, .intercepts()))
  }

  if (length(groups) == 1) {
    ylab <- groups
    dataplot$y <- 0
  } else {
    ylab <- "Parameters"
  }

  dataplot <- .fix_facet_names(dataplot)

  attr(dataplot, "info") <- list(
    "xlab" = "Possible parameter values",
    "ylab" = ylab,
    "legend_fill" = "Probability",
    "title" = "Practical Significance"
  )

  class(dataplot) <- c("data_plot", "see_p_significance", class(dataplot))
  dataplot
}



#' @keywords internal
.compute_densities_ps <- function(x, name = "Y", threshold = 0) {
  out <- .as.data.frame_density(stats::density(x))

  fifty_cents <- sum(out$y[out$x > threshold]) > (sum(out$y) / 2)

  out$fill <- "Less Probable"
  out$fill[abs(out$x) < threshold] <- "ROPE"
  out$fill[(out$x > threshold)] <- ifelse(fifty_cents, "Significant", "Less Probable")
  out$fill[out$x < (-1 * threshold)] <- ifelse(fifty_cents, "Less Probable", "Significant")

  out$height <- out$y
  out$y <- name

  # normalize
  out$height <- as.vector((out$height - min(out$height, na.rm = TRUE)) / diff(range(out$height, na.rm = TRUE), na.rm = TRUE))
  out
}



# Plot --------------------------------------------------------------------

#' Plot method for practical significance
#'
#' The `plot()` method for the `bayestestR::p_significance()` function.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_estimate_density
#' @inheritParams plot.see_parameters_model
#'
#' @return A ggplot2-object.
#'
#' @examples
#' \donttest{
#' if (require("bayestestR") && require("rstanarm")) {
#'   set.seed(123)
#'   m <<- stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0)
#'   result <- p_significance(m)
#'   plot(result)
#' }
#' }
#' @importFrom ggplot2 .data
#' @export
plot.see_p_significance <- function(x,
                                    data = NULL,
                                    show_intercept = FALSE,
                                    priors = FALSE,
                                    priors_alpha = .4,
                                    n_columns = 1,
                                    ...) {
  # save model for later use
  model <- .retrieve_data(x)

  # retrieve and prepare data for plotting
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data, show_intercept = show_intercept)
  }

  # check if we have multiple panels
  if ((!"Effects" %in% names(x) || length(unique(x$Effects)) <= 1) &&
    (!"Component" %in% names(x) || length(unique(x$Component)) <= 1)) {
    n_columns <- NULL
  }

  # get labels
  labels <- .clean_parameter_names(x$y, grid = !is.null(n_columns))

  insight::check_if_installed("ggridges")

  # base setup
  p <- ggplot(
    as.data.frame(x),
    aes(
      x = .data$x,
      y = .data$y,
      height = .data$height,
      group = .data$y,
      fill = .data$fill
    )
  ) +
    ggridges::geom_ridgeline_gradient() +
    add_plot_attributes(x)

  # add prior layer
  if (priors) {
    p <- p +
      .add_prior_layer_ridgeline(
        model,
        show_intercept = show_intercept,
        priors_alpha = priors_alpha
      ) +
      scale_fill_manual(values = c("white", "#FFFC00", "#cd201f", "#0077B5"))
  } else {
    p <- p +
      scale_fill_manual(values = c("white", "#cd201f", "#0077B5"))
  }

  p <- p +
    geom_vline(aes(xintercept = 0), linetype = "dotted") +
    guides(fill = "none", color = "none", group = "none")


  if (length(unique(x$y)) == 1 && is.numeric(x$y)) {
    p <- p + scale_y_continuous(breaks = NULL, labels = NULL)
  } else {
    p <- p + scale_y_discrete(labels = labels)
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
