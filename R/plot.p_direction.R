#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.p_direction <- function(x, data = NULL, ...){
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
    levels_order <- rev(x$Parameter)
    data <- data[, x$Parameter]
    dataplot <- data.frame()
    for (i in names(data)) {
      dataplot <- rbind(dataplot, .compute_densities_pd(data[[i]], name = i))
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

  attr(dataplot, "info") <- list("xlab" = "Possible parameter values",
                                  "ylab" = "Parameters",
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
#' @export
plot.see_p_direction <- function(x, data = NULL, show_intercept = FALSE, ...){
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
    add_plot_attributes(x) +
    geom_vline(aes(xintercept = 0), lintype = "dotted")

  p
}

