#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.hdi <- function(x, data = NULL, show_intercept = FALSE, ...) {
  .data_plot_hdi(x = x, data = data, show_intercept = show_intercept)
}

#' @export
data_plot.bayestestR_hdi <- data_plot.hdi

#' @export
data_plot.bayestestR_eti <- data_plot.hdi



#' @keywords internal
.data_plot_hdi <- function(x, data = NULL, parms = NULL, show_intercept = FALSE, ...) {
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  if (inherits(x, "bayestestR_hdi")) {
    legend_title <- "HDI"
    plot_title <- "Highest Density Interval (HDI)"
  } else {
    legend_title <- "CI"
    plot_title <- "Credible Interval (CI)"
  }

  if (!is.null(parms))
    params <- parms
  else
    params <- NULL

  if (inherits(data, "emmGrid")) {
    if (!requireNamespace("emmeans", quietly = TRUE)) {
      stop("Package 'emmeans' required for this function to work. Please install it.", call. = FALSE)
    }
    data <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(data, names = FALSE)))
  } else if (inherits(data, c("stanreg", "brmsfit"))) {
    params <- insight::clean_parameters(data)
    data <- as.data.frame(data)
  } else if (inherits(data, "BFBayesFactor")) {
    data <- insight::get_parameters(data)
  } else if (inherits(data, "MCMCglmm")) {
    nF <- data$Fixed$nfl
    data <- as.data.frame(data$Sol[, 1:nF, drop = FALSE])
  } else {
    data <- as.data.frame(data)
  }

  if (ncol(data) > 1) {
    levels_order <- unique(rev(x$Parameter))
    data <- data[, levels_order]
    dataplot <- data.frame()
    for (i in names(data)) {
      if (!is.null(params)) {
        dataplot <- rbind(
          dataplot,
          cbind(
            .compute_densities_hdi(data[[i]], hdi = as.data.frame(x[x$Parameter == i, ]), name = i),
            "Effects" = params$Effects[params$Parameter == i],
            "Component" = params$Component[params$Parameter == i]
          )
        )
      } else {
        dataplot <- rbind(
          dataplot,
          .compute_densities_hdi(data[[i]], hdi = as.data.frame(x[x$Parameter == i, ]), name = i)
        )
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
    dataplot <- .compute_densities_hdi(x = data[, 1], hdi = x, name = "Posterior")
  }

  cn <- intersect(c("x", "y", "height", "fill", "Effects", "Component"), colnames(dataplot))
  dataplot <- dataplot[, cn, drop = FALSE]

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
                                 "legend_fill" = legend_title,
                                 "title" = plot_title)

  class(dataplot) <- c("data_plot", "see_hdi", class(dataplot))
  dataplot
}




#' @importFrom rlang .data
#' @importFrom stats density
#' @importFrom dplyr mutate arrange desc
#' @keywords internal
.compute_densities_hdi <- function(x, hdi, name = "Y"){
  hdi <- dplyr::arrange(hdi, dplyr::desc(.data$CI))
  out <- x %>%
    stats::density() %>%
    .as.data.frame_density() %>%
    dplyr::mutate(HDI_low = sapply(x, .classify_hdi, hdi$CI_low, c(100, hdi$CI)),
                  HDI_high = sapply(x, .classify_hdi, rev(hdi$CI_high), c(rev(hdi$CI), 100)),
                  fill = as.factor(ifelse(.data$HDI_low > .data$HDI_high, .data$HDI_low, .data$HDI_high)),
                  height = .data$y, y = name)
  # normalize
  out$height <- as.vector((out$height - min(out$height, na.rm = TRUE)) / diff(range(out$height, na.rm = TRUE), na.rm = TRUE))
  out
}




#' @keywords internal
.classify_hdi <- function(x, breakpoints, labels, if_lower = TRUE) {
  limits <- list(
    breakpoints = breakpoints,
    labels = labels
  )

  check <- x < limits$breakpoints
  if (TRUE %in% check) {
    index <- min(which(check))
  } else {
    index <- length(limits$labels)
  }
  out <- limits$labels[index]
  out
}





# Plot --------------------------------------------------------------------
#' @importFrom ggridges geom_ridgeline_gradient
#' @importFrom rlang .data
#' @rdname data_plot
#' @export
plot.see_hdi <- function(x, data = NULL, show_intercept = FALSE, show_zero = TRUE, show_title = TRUE, n_columns = 1, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data, show_intercept = show_intercept)
  }

  # check if we have multiple panels
  if ((!"Effects" %in% names(x) || length(unique(x$Effects)) <= 1) &&
      (!"Component" %in% names(x) || length(unique(x$Component)) <= 1)) n_columns <- NULL

  # get labels
  labels <- .clean_parameter_names(x$y, grid = !is.null(n_columns))

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

  # Show
  if(show_zero){
    p <- p + geom_vline(xintercept = 0, linetype = "dotted")
  }

  if(show_title == FALSE){
    p <- p + ggtitle("")
  }

  if (length(unique(x$y)) == 1  && is.numeric(x$y)) {
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


#' @export
plot.see_eti <- plot.see_hdi
