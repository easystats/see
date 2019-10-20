#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.hdi <- function(x, data = NULL, grid = TRUE, ...) {
  .data_plot_hdi(x, data, grid)
}

#' @export
data_plot.bayestestR_hdi <- data_plot.hdi

#' @export
data_plot.bayestestR_eti <- data_plot.hdi



#' @keywords internal
.data_plot_hdi <- function(x, data = NULL, grid = TRUE, parms = NULL, ...) {
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

  # clean cryptic names
  if (grid) {
    dataplot$y <- .clean_parameter_names(dataplot$y)
    if (!is.null(levels_order)) levels_order <- .clean_parameter_names(levels_order)
  }

  if (!is.null(levels_order)) {
    dataplot$y <- factor(dataplot$y, levels = levels_order)
  }

  if (length(unique(dataplot$y)) == 1) {
    ylab <- unique(dataplot$y)
    dataplot$y <- 0
  } else {
    ylab <- "Parameters"
  }

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
plot.see_hdi <- function(x, data = NULL, show_intercept = FALSE, grid = TRUE, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  x <- .remove_intercept(x, column = "y", show_intercept)

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
    geom_vline(xintercept = 0, linetype = "dotted") +
    add_plot_attributes(x)

  if (length(unique(x$y)) == 1) {
    p <- p + scale_y_continuous(breaks = NULL, labels = NULL)
  }

  if ("Effects" %in% names(x) && isTRUE(grid)) {
    if ("Component" %in% names(x))
      p <- p + facet_wrap(~ Effects + Component, scales = "free")
    else
      p <- p + facet_wrap(~ Effects, scales = "free")
  }

  p
}


#' @export
plot.see_eti <- plot.see_hdi
