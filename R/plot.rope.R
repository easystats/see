#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.rope <- function(x, data = NULL, ...){
  if (is.null(data)) {
    data <- .retrieve_data(x)
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
  dataplot <- .data_plot_hdi(hdi, data)
  rope_range <- unique(c(x$ROPE_low, x$ROPE_high))
  if (length(rope_range) != 2) {
    stop("Only one ROPE range accepted.")
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
plot.see_rope <- function(x, data = NULL, rope_alpha = 0.5, rope_color = "cadetblue", ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
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
    annotate(
      "rect",
      xmin = attributes(x)$info$rope_range[1],
      xmax = attributes(x)$info$rope_range[2],
      ymin = 0,
      ymax = Inf,
      fill = rope_color,
      alpha = rope_alpha
    ) +
    ggridges::geom_ridgeline_gradient() +
    add_plot_attributes(x)

  p
}

