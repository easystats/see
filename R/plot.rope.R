#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.rope <- function(x, data = NULL, grid = TRUE, show_intercept = FALSE, ...){
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

  # Recontruct hdi
  hdi <- attributes(x)$HDI_area

  if (!is.data.frame(hdi)) {
    for (i in names(hdi)) {
      hdi[[i]]$Parameter <- i
    }
    hdi <- do.call("rbind", hdi)
  }

  # Extract data HDI
  dataplot <- .data_plot_hdi(hdi, data, grid, parms = params)
  rope_range <- unique(c(x$ROPE_low, x$ROPE_high))
  if (length(rope_range) != 2) {
    stop("Only one ROPE range accepted.")
  }

  groups <- unique(dataplot$y)
  if (!show_intercept) {
    dataplot <- .remove_intercept(dataplot, column = "y", show_intercept = show_intercept)

    groups <- unique(setdiff(
      groups,
      c("Intercept", "zi_Intercept", "(Intercept)", "b_Intercept", "b_zi_Intercept")
    ))
  }

  if (length(groups) == 1) {
    dataplot$y <- 0
  }

  dataplot$xmin <- rope_range[1]
  dataplot$xmax <- rope_range[2]
  dataplot$color <- ifelse(dataplot$x >= dataplot$xmin & dataplot$x <= dataplot$xmax, "Negligible", "Significant")
  attributes(dataplot)$info$rope_range <- rope_range
  attributes(dataplot)$info$title <- "Region of Practical Equivalence (ROPE)"

  class(dataplot) <- c("data_plot", "see_rope", "data.frame")
  dataplot
}







# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @importFrom rlang .data
#' @export
plot.see_rope <- function(x, data = NULL, rope_alpha = 0.5, rope_color = "cadetblue", show_intercept = FALSE, grid = TRUE, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data, show_intercept = show_intercept)
  }

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
    annotate(
      "rect",
      xmin = attributes(x)$info$rope_range[1],
      xmax = attributes(x)$info$rope_range[2],
      ymin = 0,
      ymax = Inf,
      fill = rope_color,
      alpha = rope_alpha
    ) +
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

