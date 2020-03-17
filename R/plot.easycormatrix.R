#' @export
data_plot.see_easycormatrix <- function(x, data = NULL, ...) {
  data <- as.data.frame(x)
  dataplot <- .reshape_to_long(data, names_to = "Parameter", values_to = "r", columns = 2:ncol(data))

  data <- as.data.frame(attr(x, "p", exact = TRUE))
  dataplot_p <- .reshape_to_long(data, names_to = "Parameter", values_to = "p", columns = 2:ncol(data))

  n_param <- length(unique(dataplot$Parameter))
  x <- rep(1:n_param, length.out = nrow(dataplot))
  y <- rep(1:n_param, each = n_param)

  dataplot$x <- unique(dataplot$Parameter)[x]
  dataplot$y <- unique(dataplot$Parameter)[y]

  dataplot$Parameter <- factor(dataplot$Parameter, levels = unique(dataplot$Parameter))
  dataplot$x <- factor(dataplot$x, levels = unique(dataplot$x))
  dataplot$y <- factor(dataplot$y, levels = unique(dataplot$y))

  dataplot$r[abs(dataplot$r) > .99999] <- NA
  dataplot$p <- dataplot_p$p

  dataplot$labels <- sprintf("%.3f", dataplot$r)
  dataplot$labels[dataplot$labels == "NA"] <- ""

  class(dataplot) <- unique(c("data_plot", "see_easycormatrix", class(dataplot)))
  dataplot
}




# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @importFrom parameters format_p
#' @export
plot.see_easycormatrix <- function(x, show_values = FALSE, show_p = FALSE, show_legend = TRUE, text_size = 3.5, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
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
    theme(axis.line = element_blank())

  if (show_values) {
    p <- p + geom_text(aes(label = .data$labels), size = text_size, colour = "black")
  }

  if (!show_legend) {
    p <- p + guides(fill = "none")
  }

  p
}
