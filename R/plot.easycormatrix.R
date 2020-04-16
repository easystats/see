#' @export
data_plot.see_easycormatrix <- function(x, data = NULL, digits = 3, ...) {
  legend_fill <- attr(x, "coefficient_name")
  redundant <- attr(x, "redundant")

  data <- as.data.frame(x)
  dataplot <- .reshape_to_long(data, names_to = "Parameter", values_to = "r", columns = 2:ncol(data))

  if (!redundant) {
    dataplot$y <- rep(data$Parameter, times = nrow(data))
    dataplot$y <- factor(dataplot$y, levels = rev(unique(dataplot$y)))
    dataplot$x <- dataplot$Parameter <- factor(dataplot$Parameter, levels = unique(dataplot$Parameter))
  } else {
    unique_param <- unique(dataplot$Parameter)
    n_param <- length(unique_param)
    x_lab <- rep(1:n_param, length.out = nrow(dataplot))
    y_lab <- rep(1:n_param, each = n_param)

    dataplot$x <- unique_param[x_lab]
    dataplot$y <- unique_param[y_lab]

    dataplot$Parameter <- factor(dataplot$Parameter, levels = rev(unique_param))
    dataplot$x <- factor(dataplot$x, levels = unique_param)
    dataplot$y <- factor(dataplot$y, levels = rev(unique_param))
  }

  data <- as.data.frame(attr(x, "p", exact = TRUE))
  if (nrow(data) > 0) {
    dataplot_p <- .reshape_to_long(data, names_to = "Parameter", values_to = "p", columns = 2:ncol(data))
    dataplot$p <- dataplot_p$p
  }

  dataplot$r[abs(dataplot$r) > .99999] <- NA

  dataplot$labels <- sprintf("%.*f", digits, dataplot$r)
  dataplot$labels[dataplot$labels == "NA"] <- ""

  attr(dataplot, "info") <- list("legend_fill" = legend_fill)
  attr(dataplot, "redundant") <- redundant
  class(dataplot) <- unique(c("data_plot", "see_easycormatrix", class(dataplot)))
  dataplot
}




# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @importFrom parameters format_p
#' @export
plot.see_easycormatrix <- function(x, show_values = FALSE, show_p = FALSE, show_legend = TRUE, text_size = 3.5, digits = 3, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, digits = digits)
  }

  if (show_p) {
    non_empty <- x$labels != ""
    x$labels[non_empty] <- paste0(
      x$labels[non_empty],
      parameters::format_p(x$p, stars_only = TRUE)[non_empty]
    )
  }

  p <- ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$r)) +
    geom_tile(size = 0, colour = NA) +
    scale_fill_gradientn(
      colours = c("#c0392b", "white", "#2980b9"),
      limits = c(-1, 1),
      na.value = "white"
    ) +
    labs(x = NULL, y = NULL) +
    theme_modern() +
    theme(axis.line = element_blank()) +
    add_plot_attributes(x)

  if (show_values) {
    p <- p + geom_text(aes(label = .data$labels), size = text_size, colour = "black")
  }

  if (!show_legend) {
    p <- p + guides(fill = "none")
  }

  if (!isTRUE(attributes(x)$redundant)) {
    p <- p + scale_x_discrete(position = "top")
  }

  p
}
