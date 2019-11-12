#' @importFrom insight clean_parameters
#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.p_direction <- function(x, data = NULL, show_intercept = FALSE, ...){
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  params <- NULL

  if (inherits(data, "emmGrid")) {
    if (!requireNamespace("emmeans", quietly = TRUE)) {
      stop("Package 'emmeans' required for this function to work. Please install it.", call. = FALSE)
    }
    data <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(data, names = FALSE)))
  } else if (inherits(data, c("stanreg", "brmsfit"))) {
    params <- insight::clean_parameters(data)
    data <- as.data.frame(data)
  } else {
    data <- as.data.frame(data)
  }

  if (ncol(data) > 1) {
    levels_order <- rev(x$Parameter)
    data <- data[, x$Parameter]
    dataplot <- data.frame()
    for (i in names(data)) {
      if (!is.null(params)) {
        dataplot <- rbind(
          dataplot,
          cbind(
            .compute_densities_pd(data[[i]], name = i),
            "Effects" = params$Effects[params$Parameter == i],
            "Component" = params$Component[params$Parameter == i]
          )
        )
      } else {
        dataplot <- rbind(dataplot, .compute_densities_pd(data[[i]], name = i))
      }
    }

    if ("Effects" %in% names(dataplot) && "Component" %in% names(dataplot)) {
      if (length(unique(dataplot$Effects)) == 1 && length(unique(dataplot$Component)) == 1) {
        dataplot$Effects <- NULL
        dataplot$Component <- NULL
      } else {
        dataplot$Effects <- factor(dataplot$Effects, levels = sort(levels(dataplot$Effects)))
        dataplot$Component <- factor(dataplot$Component, levels = sort(levels(dataplot$Component)))
      }
    }

  } else {
    levels_order <- NULL
    dataplot <- .compute_densities_pd(data[, 1], name = "Posterior")
  }

  dataplot <- dataplot %>%
    dplyr::group_by(.data$y, .data$fill) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$y) %>%
    dplyr::mutate(prop = .data$n / dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fill2 = ifelse(.data$prop >= .5, "Most probable", "Less probable")) %>%
    dplyr::select(-dplyr::one_of("n", "prop"))

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

  attr(dataplot, "info") <- list("xlab" = "Possible parameter values",
                                  "ylab" = ylab,
                                  "legend_fill" = "Effect direction",
                                  "title" = "Probability of Direction")

  class(dataplot) <- c("data_plot", "see_p_direction", class(dataplot))
  dataplot
}



#' @importFrom rlang .data
#' @importFrom stats density
#' @importFrom dplyr mutate
#' @keywords internal
.compute_densities_pd <- function(x, name = "Y"){
  out <- x %>%
    stats::density() %>%
    .as.data.frame_density() %>%
    dplyr::mutate(fill = ifelse(.data$x < 0, "Negative", "Positive")) %>%
    dplyr::mutate(height = .data$y, y = name)

  # normalize
  out$height <- as.vector((out$height - min(out$height, na.rm = TRUE)) / diff(range(out$height, na.rm = TRUE), na.rm = TRUE))
  out
}



# Plot --------------------------------------------------------------------
#' @importFrom rlang .data
#' @importFrom ggridges geom_ridgeline_gradient
#' @rdname data_plot
#' @export
plot.see_p_direction <- function(x, data = NULL, show_intercept = FALSE, priors = FALSE, priors_alpha = .4, n_columns = 1, ...) {
  # save model for later use
  model <- .retrieve_data(x)

  # retrieve and prepare data for plotting
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data, show_intercept = show_intercept)
  }

  # check if we have multiple panels
  if ((!"Effects" %in% names(x) || length(unique(x$Effects)) <= 1) &&
      (!"Component" %in% names(x) || length(unique(x$Component)) <= 1)) n_columns <- NULL

  # get labels
  labels <- .clean_parameter_names(x$y, grid = !is.null(n_columns))

  # base setup
  p <- x %>%
    as.data.frame() %>%
    ggplot(aes(
      x = .data$x,
      y = .data$y,
      height = .data$height,
      group = .data$y,
      fill = .data$fill
    )) +
    ggridges::geom_ridgeline_gradient() +
    add_plot_attributes(x)

  # add prior layer
  if (priors) {
    p <- p + .add_prior_layer_ridgeline(
      model,
      show_intercept = show_intercept,
      priors_alpha = priors_alpha
    )
  }

  p <- p + geom_vline(aes(xintercept = 0), linetype = "dotted")


  if (length(unique(x$y)) == 1 && is.numeric(x$y)) {
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

