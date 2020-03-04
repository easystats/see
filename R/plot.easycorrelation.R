# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @importFrom utils sessionInfo
#' @export
plot.see_easycorrelation <- function(x, size = 22, text_color = "white", node_color = "#647687", ...) {
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' required for this function to work. Please install it.", call. = FALSE)
  } else {
    si <- utils::sessionInfo()
    other_packages <- names(si$otherPkgs)
    if (!is.null(other_packages) && !("ggraph" %in% other_packages)) {
      message("Package 'ggraph' needs to be loaded. Please load it by typing 'library(ggraph)' into the console.")
      return(NULL)
    }
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
