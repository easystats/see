# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @export
plot.see_easycorrelation <- function(x, size = 22, text_color = "white", node_color = "#647687", ...) {
  if (!require("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' required for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package 'tidygraph' required for this function to work. Please install it.", call. = FALSE)
  }

  data <- tidygraph::as_tbl_graph(x)

  suppressWarnings(
    ggraph::ggraph(data, layout = "kk") +
      ggraph::geom_edge_arc(aes(colour = .data$r, edge_width = abs(.data$r)), strength = 0.1) +
      ggraph::geom_node_point(color = node_color, size = size) +
      ggraph::geom_node_text(aes(label = .data$name), colour = text_color) +
      ggraph::scale_edge_color_gradient2(low = "#a20025", high = "#008a00", name = "r") +
      ggraph::theme_graph() +
      guides(edge_width = FALSE) +
      scale_x_continuous(expand = expand_scale(c(.10, .10))) +
      scale_y_continuous(expand = expand_scale(c(.10, .10)))
  )
}
