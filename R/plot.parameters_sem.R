#' @export
data_plot.parameters_sem <- function(x, data = NULL, type = c("regression", "correlation", "loading"), threshold_coefficient = NULL, threshold_p = NULL, ci = TRUE, ...){

  # Deal with thresholds
  if (is.null(threshold_coefficient)) {
    threshold_coefficient <- min(abs(x$Coefficient)) - 1
  }
  if (is.null(threshold_p)) {
    threshold_p <- 1.1
  }

  # Edge properties
  edges <- x
  edges$Coefficient_abs <- abs(x$Coefficient)
  edges$from <- as.character(x$From)
  edges$to <- as.character(x$To)

  edges <- edges[tolower(edges$Type) %in% type &
                   edges$from != edges$to &
                   edges$Coefficient_abs >= threshold_coefficient &
                   edges$p < threshold_p, ]

  edges$Coefficient_abs <- NULL
  edges$From <- NULL
  edges$To <- NULL

  rownames(edges) <- NULL

  # edges <- x %>%
  #   dplyr::mutate(Coefficient_abs = abs(.data$Coefficient),
  #                 From = as.character(.data$From),
  #                 To = as.character(.data$To)) %>%
  #   dplyr::filter(
  #     tolower(.data$Type) %in% c(type),
  #     .data$From != .data$To,
  #     .data$Coefficient_abs >= threshold_coefficient,
  #     .data$p < threshold_p
  #   ) %>%
  #   dplyr::select(-dplyr::one_of("Coefficient_abs")) %>%
  #   dplyr::rename(
  #     "to" = "To",
  #     "from" = "From"
  #   )

  # Labels
  if (ci == TRUE) {
    edges$Label <- paste0(
      sprintf("%.2f, ", edges$Coefficient),
      attributes(x)$ci * 100,
      "% CI [",
      sprintf("%.2f, %.2f]", edges$CI_low, edges$CI_high))
  } else {
    edges$Label <- sprintf("%.2f", edges$Coefficient)
  }



  # Separate labels
  edges$Label_Regression <- ifelse(edges$Type == 'Regression', edges$Label, '')
  edges$Label_Correlation <- ifelse(edges$Type == 'Correlation', edges$Label, '')
  edges$Label_Loading <- ifelse(edges$Type == 'Loading', edges$Label, '')
  edges <- edges[colSums(!is.na(edges)) > 0]

  # Identify latent variables for nodes
  latent_nodes <- edges %>%
    dplyr::filter(.data$Type == "Loading") %>%
    dplyr::distinct(.data$to) %>%
    dplyr::transmute(Name = as.character(.data$to), Latent = TRUE)



  # Node
  nodes <- data.frame(Name = unique(c(as.character(edges$from), as.character(edges$to))), stringsAsFactors = FALSE) %>%
    dplyr::left_join(latent_nodes, by = "Name") %>%
    dplyr::mutate(Latent = ifelse(is.na(.data$Latent), FALSE, .data$Latent))


  dataplot <- list(edges = edges, nodes = nodes)
  class(dataplot) <- c("data_plot", "see_parameters_sem", class(dataplot))
  dataplot
}




# Plot --------------------------------------------------------------------
#' @importFrom rlang .data
#' @rdname data_plot
#' @export
plot.see_parameters_sem <- function(x, data = NULL, type = c("regression", "correlation", "loading"), threshold_coefficient = NULL, threshold_p = NULL, ci = TRUE, size = 22, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, type = type, threshold_coefficient = threshold_coefficient, threshold_p = threshold_p, ci = ci, ...)
  }

  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' required for this function to work. Please install it by running `install.packages('ggraph')`.")
  }

  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package 'tidygraph' required for this function to work. Please install it by running `install.packages('tidygraph')`.")
  }

  p <- tidygraph::tbl_graph(x$nodes, x$edges) %>%
    ggraph::ggraph(layout = "nicely") +
    ggraph::geom_edge_arc(aes(alpha = as.numeric(.data$Type == "Correlation"),
                      label = .data$Label_Correlation,
                      color = .data$Coefficient),
                      strength = 0.1,
                      label_dodge = unit(2, "mm"),
                      linetype = 2, angle_calc = "along",
                      label_size = 3,
                      start_cap = ggraph::circle(12, 'mm'), end_cap = ggraph::circle(12, 'mm')) +
    ggraph::geom_edge_link(aes(alpha = as.numeric(.data$Type == "Loading"),
                       label = .data$Label_Loading,
                       color = .data$Coefficient),
                       label_dodge = unit(2, "mm"),
                       angle_calc = "along", edge_width = 1,
                       label_size = 3,
                       check_overlap = TRUE,
                       arrow = arrow(type = "closed", length = unit(3, "mm")),
                       start_cap = ggraph::circle(12, 'mm'), end_cap = ggraph::circle(12, 'mm')) +
    ggraph::geom_node_point(aes(colour = .data$Latent), size = size) +
    ggraph::geom_node_text(aes(label = .data$Name))  +
    ggraph::scale_edge_colour_gradient2(
      guide = FALSE,
      high = "#4CAF50",
      mid = "#FFF9C4",
      low = "#E91E63"
    ) +
    scale_alpha(guide = FALSE, range = c(0, 1)) +
    ggraph::scale_edge_alpha(guide = FALSE, range = c(0, 1)) +
    scale_x_continuous(expand = expansion(c(.10, .10))) +
    scale_y_continuous(expand = expansion(c(.10, .10))) +
    ggraph::theme_graph() +
    theme(legend.position = "none")

  p
}

