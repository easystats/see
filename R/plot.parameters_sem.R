#' @export
data_plot.parameters_sem <- function(
  x,
  data = NULL,
  component = c("regression", "correlation", "loading"),
  type = component,
  threshold_coefficient = NULL,
  threshold_p = NULL,
  ci = TRUE,
  ...
) {
  # Compatibility patch
  if (!all(type %in% component)) {
    component <- type
  }

  # Deal with thresholds
  if (is.null(threshold_coefficient)) {
    threshold_coefficient <- min(abs(x$Coefficient)) - 1
  }
  if (is.null(threshold_p)) {
    threshold_p <- 1.1
  }

  # Edge properties
  edges <- as.data.frame(x)
  edges$Coefficient_abs <- abs(x$Coefficient)
  edges$from <- as.character(x$From)
  edges$to <- as.character(x$To)

  # Revert order of arrows for loadings
  # It is the latent factor that manifests itself in the indicators (#95)
  edges[edges$Component == "Loading", "from"] <- as.character(edges[
    edges$Component == "Loading",
    "To"
  ])
  edges[edges$Component == "Loading", "to"] <- as.character(edges[
    edges$Component == "Loading",
    "From"
  ])

  edges <- edges[
    tolower(edges$Component) %in%
      component &
      edges$from != edges$to &
      edges$Coefficient_abs >= threshold_coefficient &
      edges$p < threshold_p,
  ]

  edges$Coefficient_abs <- NULL
  edges$From <- NULL
  edges$To <- NULL

  rownames(edges) <- NULL

  # Labels
  if (ci) {
    edges$Label <- paste0(
      sprintf("%.2f, ", edges$Coefficient),
      attributes(x)$ci * 100,
      "% CI [",
      sprintf("%.2f, %.2f]", edges$CI_low, edges$CI_high)
    )
  } else {
    edges$Label <- sprintf("%.2f", edges$Coefficient)
  }

  # Separate labels
  edges$Label_Regression <- ifelse(
    edges$Component == "Regression",
    edges$Label,
    ""
  )
  edges$Label_Correlation <- ifelse(
    edges$Component == "Correlation",
    edges$Label,
    ""
  )
  edges$Label_Loading <- ifelse(edges$Component == "Loading", edges$Label, "")
  edges <- edges[colSums(!is.na(edges)) > 0L]

  # Identify nodes
  latent_nodes <- data.frame(
    Name = as.character(edges[edges$Component == "Loading", "to"]),
    Latent = TRUE,
    stringsAsFactors = FALSE
  )
  manifest_nodes <- data.frame(
    Name = unique(c(edges$from, edges$to)),
    Latent = FALSE,
    stringsAsFactors = FALSE
  )
  manifest_nodes <- manifest_nodes[
    !manifest_nodes$Name %in% latent_nodes$Name,
  ]
  nodes <- rbind(manifest_nodes, latent_nodes)

  dataplot <- list(edges = edges, nodes = nodes)
  class(dataplot) <- c("data_plot", "see_parameters_sem", class(dataplot))
  dataplot
}


# Plot --------------------------------------------------------------------
#' @param threshold_coefficient Numeric, threshold at which value coefficients will be displayed.
#' @param threshold_p Numeric, threshold at which value p-values will be displayed.
#' @param ci Logical, whether confidence intervals should be added to the plot.
#'
#' @rdname plot.see_parameters_model
#' @export
plot.see_parameters_sem <- function(
  x,
  data = NULL,
  component = c("regression", "correlation", "loading"),
  type = component,
  threshold_coefficient = NULL,
  threshold_p = NULL,
  ci = TRUE,
  size_point = 22,
  ...
) {
  if (!inherits(x, "data_plot")) {
    x <- data_plot(
      x,
      component = component,
      type = type,
      threshold_coefficient = threshold_coefficient,
      threshold_p = threshold_p,
      ci = ci,
      ...
    )
  }

  if (!requireNamespace("ggraph", quietly = TRUE)) {
    insight::check_if_installed("ggraph")
  }

  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    insight::check_if_installed("tidygraph")
  }

  p <- ggraph::ggraph(tidygraph::tbl_graph(x$nodes, x$edges), ...) +

    # Plot Correlations
    ggraph::geom_edge_arc(
      aes(
        alpha = as.numeric(.data$Component == "Correlation"),
        label = .data$Label_Correlation,
        color = .data$Coefficient
      ),
      strength = 0.1,
      label_dodge = unit(2, "mm"),
      linetype = 2,
      angle_calc = "along",
      label_size = 3,
      start_cap = ggraph::circle(12, "mm"),
      end_cap = ggraph::circle(12, "mm")
    ) +
    # Plot Loadings
    ggraph::geom_edge_link(
      aes(
        alpha = as.numeric(.data$Component == "Loading"),
        label = .data$Label_Loading,
        color = .data$Coefficient
      ),
      label_dodge = unit(2, "mm"),
      angle_calc = "along",
      edge_width = 0.8,
      label_size = 3,
      arrow = arrow(type = "closed", length = unit(3, "mm")),
      start_cap = ggraph::circle(12, "mm"),
      end_cap = ggraph::circle(12, "mm")
    ) +
    # Plot regressions
    ggraph::geom_edge_link(
      aes(
        alpha = as.numeric(.data$Component == "Regression"),
        label = .data$Label_Regression,
        color = .data$Coefficient
      ),
      label_dodge = unit(2, "mm"),
      angle_calc = "along",
      edge_width = 1.2,
      label_size = 3,
      arrow = arrow(type = "closed", length = unit(3, "mm")),
      start_cap = ggraph::circle(12, "mm"),
      end_cap = ggraph::circle(12, "mm")
    ) +
    ggraph::geom_node_point(
      aes(colour = .data$Latent, shape = .data$Latent),
      size = size_point
    ) +
    ggraph::geom_node_text(aes(label = .data$Name)) +
    ggraph::scale_edge_colour_gradient2(
      guide = "none",
      high = "#4CAF50",
      mid = "#FFF9C4",
      low = "#E91E63"
    ) +
    scale_alpha(guide = "none", range = c(0, 1)) +
    scale_shape_manual(values = c(`FALSE` = 15, `TRUE` = 19)) +
    ggraph::scale_edge_alpha(guide = "none", range = c(0, 1)) +
    scale_x_continuous(expand = expansion(c(0.10, 0.10))) +
    scale_y_continuous(expand = expansion(c(0.10, 0.10))) +
    ggraph::theme_graph() +
    theme(legend.position = "none")

  p
}
