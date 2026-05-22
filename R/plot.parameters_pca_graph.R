.plot_pca_as_graph <- function(
  object,
  loading_text_size = 2.8,
  arrow_end_gap = 0.10,
  factor_node_size = c(22, 35),
  expand = c(0.5, 0.5),
  names_factors = NULL,
  color_variables = "#738b8d",
  color_factors = "#3b4f64",
  ...
) {
  insight::check_if_installed(c("ggraph", "tidygraph"))

  # 1. Gather info and clean data ---------------------------------------------
  # These columns are standard outputs in easystats PCA/FA objects that we don't
  # want to plot as factors.
  meta_cols <- c("Complexity", "Uniqueness", "MSA", "Mean", "SD")
  threshold <- .default_value(object, "threshold", 0.3)

  # 1. Extract ALL loadings first

  # Remove metadata columns and melt the dataframe from wide to long format.
  # This creates a structure where each row represents a single
  # variable-to-factor relationship.
  df_wide <- datawizard::data_remove(as.data.frame(object), meta_cols)

  df_all <- datawizard::data_to_long(
    df_wide,
    select = -Variable,
    names_to = "Factor",
    values_to = "Loading"
  )

  # 2. Process node metadata (Colors and Ordering) ----------------------------
  # Process Variables (Color & Order)
  var_processed <- .process_colors_and_order(
    unique(df_all$Variable),
    color_variables,
    "#95A5A6"
  )
  variables <- var_processed$items
  var_colors <- var_processed$colors
  n_var <- length(variables)

  # Process Factors (Color & Order)
  fac_processed <- .process_colors_and_order(
    unique(df_all$Factor),
    color_factors,
    "#2C3E50"
  )
  original_factors <- fac_processed$items
  fac_colors <- fac_processed$colors
  n_fac <- length(original_factors)

  # 3. Handle custom factor names ---------------------------------------------
  # Allows the user to rename factors (e.g., replacing "Component 1" with
  # "Extraversion")
  display_factors <- stats::setNames(original_factors, original_factors)

  if (!is.null(names_factors)) {
    if (all(names(names_factors) %in% original_factors)) {
      # We know for sure names() are the old factors
      display_factors[names(names_factors)] <- unlist(names_factors)
    } else {
      not_valid <- setdiff(names(names_factors), original_factors)
      insight::format_error(
        "Names of the `names_factors` vector must match existing factor names.",
        paste0(
          "These are ",
          datawizard::text_concatenate(original_factors, enclose = "`"),
          ". You provided ",
          datawizard::text_concatenate(not_valid, enclose = "`"),
          "."
        )
      )
    }
  }

  # 4. Extract or calculate variance explained --------------------------------
  # Variance explained dictates the size of the factor nodes.
  var_attr <- attributes(object)$variance

  if (!is.null(var_attr)) {
    if (is.numeric(var_attr)) {
      prop_var <- var_attr
    } else if (is.data.frame(var_attr) && "Variance" %in% names(var_attr)) {
      prop_var <- var_attr$Variance
    }

    # Map variance values to the correct factors
    if (is.null(names(prop_var)) && length(prop_var) >= n_fac) {
      names(prop_var) <- original_factors
    }
  } else {
    # If not provided, manually calculate proportion of variance:
    # Sum of squared loadings divided by total number of variables.
    ss_loadings <- tapply(df_all$Loading^2, df_all$Factor, sum)
    prop_var <- as.numeric(ss_loadings / n_var)
    names(prop_var) <- names(ss_loadings)
  }

  # Normalize percentages to proportions if needed
  if (max(prop_var, na.rm = TRUE) > 1) {
    prop_var <- prop_var / 100
  }

  # 5. Define Graph Edges -----------------------------------------------------
  # Filter out weak cross-loadings below the threshold. Rename columns to
  # strictly match ggraph's expected edge format (from, to, weight).
  edges_subset <- subset(df_all, abs(Loading) >= threshold)
  edges <- datawizard::data_rename(
    edges_subset,
    pattern = c("Variable", "Factor", "Loading"),
    replacement = c("from", "to", "weight")
  )

  # 6. Build Graph Nodes & Coordinates ----------------------------------------
  # Manual bipartite layout:
  # Variables get x = 0 (left column), descending y-coordinates.
  # Factors get x = 1 (right column), descending y-coordinates, slightly offset.
  y_var <- seq(n_var, 1)

  y_fac <- seq(
    from = n_var - 0.5,
    to = 1.5,
    length.out = n_fac
  )

  df_factors <- data.frame(
    name = original_factors,
    type = "Factor",
    x = 1,
    y = y_fac,
    variance = prop_var[original_factors],
    label_text = sprintf(
      "%s\n(%.1f%%)",
      display_factors,
      prop_var[original_factors] * 100
    ),
    node_color = fac_colors
  )

  df_variables <- data.frame(
    name = variables,
    type = "Variable",
    x = 0,
    y = y_var,
    variance = NA,
    label_text = variables,
    node_color = var_colors
  )

  nodes <- rbind(df_factors, df_variables)

  # 7. Render Graph -----------------------------------------------------------
  graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

  # Plot using ggraph
  ggraph::ggraph(graph, layout = "manual", x = x, y = y) +

    # -- EDGES --
    ggraph::geom_edge_link(
      ggplot2::aes(
        edge_width = abs(weight),
        edge_alpha = abs(weight),
        color = weight,
        # Psychometric standard: drop the leading zero for numbers strictly between -1 and 1
        label = sub("^(-?)0\\.", "\\1.", sprintf("%.2f", weight))
      ),
      arrow = ggplot2::arrow(length = unit(4, 'mm'), type = "closed"),
      start_cap = ggraph::circle(0, 'mm'),
      end_cap = ggraph::circle(arrow_end_gap, 'snpc'),
      angle_calc = 'along',
      label_dodge = unit(2.5, 'mm'),
      label_size = loading_text_size
    ) +

    # -- FACTOR NODES --
    ggraph::geom_node_point(
      ggplot2::aes(
        filter = type == "Factor",
        size = variance,
        fill = node_color
      ),
      shape = 21,
      color = "white",
      stroke = 1.5,
      show.legend = FALSE
    ) +

    # -- FACTOR TEXT --
    ggraph::geom_node_text(
      ggplot2::aes(filter = type == "Factor", label = label_text),
      color = "white",
      fontface = "bold",
      size = 3.5,
      lineheight = 0.9
    ) +

    # -- VARIABLE NODES --
    ggraph::geom_node_label(
      ggplot2::aes(
        filter = type == "Variable",
        label = label_text,
        fill = node_color
      ),
      color = "white",
      fontface = "bold",
      size = 3.5,
      hjust = 1,
      label.padding = ggplot2::unit(0.5, "lines"),
      show.legend = FALSE
    ) +

    # -- SCALES & AESTHETICS --
    ggplot2::scale_fill_identity() + # Evaluates our hex colors natively
    ggplot2::scale_size_continuous(range = factor_node_size, guide = "none") +
    ggraph::scale_edge_color_gradient2(
      low = "#cd201f",
      # low = "#E74C3C",
      # mid = "grey85",
      mid = "white",
      # high = "#2ECC71",
      high = "#0077B5",
      midpoint = 0,
      guide = "none"
    ) +
    ggraph::scale_edge_width_continuous(range = c(0.5, 2.5), guide = "none") +
    ggraph::scale_edge_alpha_continuous(range = c(0.4, 1), guide = "none") +

    # -- CANVAS EXPANSION & THEME --
    ggplot2::scale_x_continuous(expand = expansion(add = expand)) +
    ggplot2::coord_cartesian(clip = "off") +

    ggraph::theme_graph() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    ) +
    ggplot2::labs(title = "Factor Analysis Loadings")
}


# Helper function to process colors and reorder nodes
.process_colors_and_order <- function(items, color_input, default_color) {
  if (is.null(color_input)) {
    return(list(items = items, colors = rep(default_color, length(items))))
  }

  if (is.list(color_input)) {
    color_input <- unlist(color_input)
  }

  # 1. Handle named list/vector (e.g., c("Var3" = "red", "Var1" = "blue"))
  if (!is.null(names(color_input))) {
    input_names <- names(color_input)

    # Match against existing nodes
    valid_names <- input_names[input_names %in% items]
    missing_items <- setdiff(items, valid_names)

    # Reorder: Listed items first, missing items follow
    ordered_items <- c(valid_names, missing_items)

    # Assign colors based on new order
    mapped_colors <- color_input[ordered_items]
    mapped_colors[is.na(mapped_colors)] <- default_color # Fallback for missing

    return(list(items = ordered_items, colors = unname(mapped_colors)))
  }

  # 2. Handle single color value
  if (length(color_input) == 1) {
    return(list(items = items, colors = rep(color_input, length(items))))
  }

  # 3. Handle unnamed vector of matching length
  if (length(color_input) == length(items)) {
    return(list(items = items, colors = color_input))
  }

  # Fallback
  insight::format_alert(
    "Color vector length does not match number of nodes. Using default color."
  )
  return(list(items = items, colors = rep(default_color, length(items))))
}
