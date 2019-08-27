#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.eti <- function(x, data = NULL, ...){
  .data_plot_eti(x, data)
}

#' @export
data_plot.bayestestR_eti <- data_plot.eti


#' @keywords internal
.data_plot_eti <- function(x, data=NULL, ...){
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  if (inherits(data, "emmGrid")) {
    if (!requireNamespace("emmeans", quietly = TRUE)) {
      stop("Package 'emmeans' required for this function to work. Please install it.", call. = FALSE)
    }
    data <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(data, names = FALSE)))
  } else {
    data <- as.data.frame(data)
  }

  if (ncol(data) > 1) {
    levels_order <- unique(rev(x$Parameter))
    data <- data[, levels_order]
    dataplot <- data.frame()
    for (i in names(data)) {
      dataplot <- rbind(
        dataplot,
        .compute_densities_ci(data[[i]], ci = as.data.frame(x[x$Parameter == i, ]), name = i)
      )
    }
  } else {
    levels_order <- NULL
    dataplot <- .compute_densities_ci(x = data[, 1], ci = x, name = "Posterior")
  }

  dataplot <- dataplot %>%
    dplyr::select(dplyr::one_of("x", "y", "height", "fill"))

  if (!is.null(levels_order)) {
    dataplot$y <- factor(dataplot$y, levels = levels_order)
  }

  attr(dataplot, "info") <- list("xlab" = "Possible parameter values",
                                 "ylab" = "Parameters",
                                 "legend_fill" = "CI",
                                 "title" = "Credible Interval (CI)")

  class(dataplot) <- c("data_plot", "see_eti", class(dataplot))
  dataplot
}


#' @importFrom rlang .data
#' @importFrom stats density
#' @importFrom dplyr mutate arrange desc
#' @keywords internal
.compute_densities_ci <- function(x, ci, name = "Y"){
  ci <- dplyr::arrange(ci, dplyr::desc(.data$CI))
  out <- x %>%
    stats::density() %>%
    .as.data.frame_density() %>%
    dplyr::mutate(CI_low = sapply(x, .classify_ci, ci$CI_low, c(100, ci$CI)),
                  CI_high = sapply(x, .classify_ci, rev(ci$CI_high), c(rev(ci$CI), 100)),
                  fill = as.factor(ifelse(.data$CI_low > .data$CI_high, .data$CI_low, .data$CI_high)),
                  height = .data$y, y = name)
  # normalize
  out$height <- as.vector((out$height - min(out$height, na.rm = TRUE)) / diff(range(out$height, na.rm = TRUE), na.rm = TRUE))
  out
}




#' @keywords internal
.classify_ci <- function(x, breakpoints, labels, if_lower = TRUE) {
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
#' @export
plot.see_eti <- function(x, data = NULL, show_intercept = FALSE, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  x <- .remove_intercept(x, column = "y", show_intercept = show_intercept)

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

  p
}
