#' @importFrom bayestestR reshape_ci
#' @rdname data_plot
#' @export
plot.see_parameters_model <- function(x, show_intercept = FALSE, point_size = .8, sort = NULL, n_columns = NULL, ...) {
  if (!any(grepl("Coefficient", colnames(x), fixed = TRUE))) {
    colnames(x)[which.min(match(colnames(x), c("Median", "Mean", "Map")))] <- "Coefficient"
  }

  # is exp?
  exponentiated_coefs <- isTRUE(attributes(x)$exponentiate)
  y_intercept <- ifelse(exponentiated_coefs, 1, 0)

  # ordinal model? needed for free facet scales later...
  ordinal_model <- isTRUE(attributes(x)$ordinal_model)

  # remember components
  has_effects <- "Effects" %in% colnames(x) && length(unique(x$Effects)) > 1
  has_component <- "Component" %in% colnames(x) && length(unique(x$Component)) > 1
  has_response <- "Response" %in% colnames(x) && length(unique(x$Response)) > 1
  has_subgroups <- "Subgroup" %in% colnames(x) && length(unique(x$Subgroup)) > 1

  if ("Subgroup" %in% colnames(x)) {
    x$Subgroup[is.na(x$Subgroup)] <- ""
    x$Subgroup[x$Subgroup == "Overall"] <- ""
    x$Subgroup <- factor(x$Subgroup, levels = unique(x$Subgroup))
  }

  # do we have prettified names?
  pretty_names <- attributes(x)$pretty_names

  x <- .fix_facet_names(x)

  mc <- attributes(x)$model_class
  cp <- attributes(x)$cleaned_parameters
  is_meta <- !is.null(mc) && mc %in% c("rma", "rma.uni")

  # minor fixes for Bayesian models
  if (!is.null(mc) && !is.null(cp) && mc %in% c("stanreg", "stanmvreg", "brmsfit")) {
    x$Parameter <- cp
  }

  # data preparation for metafor-objects
  if (is_meta) {
    overall <- which(x$Parameter == "(Intercept)")
    if (length(overall) == 0) overall <- which(x$Parameter == "Overall")
    x$Parameter[overall] <- "Overall"
    x$group <- "study"
    x$group[overall] <- "Overall"
    x$point_size <- sqrt(x$Weight)
    x$point_size[overall] <- 8
    x$shape <- 19
    x$shape[overall] <- 18
  }

  # if we have a model with multiple responses or response levels
  # remove name of level from parameter name, as we split the plot
  # by response level anyway...
  if (has_response) {
    for (i in rev(sort(unique(x$Response)))) {
      x$Parameter <- gsub(i, "", x$Parameter)
      names(pretty_names) <- gsub(i, "", names(pretty_names))
    }
    x$Parameter <- gsub("^\\:(.*)", "\\1", x$Parameter)
    names(pretty_names) <- gsub("^\\:(.*)", "\\1", names(pretty_names))
  }

  # make sure components are sorted in original order, not alphabetically
  if (has_effects) {
    x$Effects <- factor(x$Effects, levels = unique(x$Effects))
  }
  if (has_component) {
    x$Component <- factor(x$Component, levels = unique(x$Component))
  }


  if (!show_intercept) {
    x <- x[!.in_intercepts(x$Parameter), ]
  }

  if (isTRUE(sort) || (!is.null(sort) && sort == "ascending")) {
    x$Parameter <- factor(x$Parameter, levels = rev(unique(x$Parameter)[order(x$Coefficient)]))
  } else if (!is.null(sort) && sort == "descending") {
    x$Parameter <- factor(x$Parameter, levels = unique(x$Parameter)[order(x$Coefficient)])
  } else {
    # sort coefficients as they appear in the classical summary output by default
    x$Parameter <- factor(x$Parameter, levels = rev(unique(x$Parameter)))
  }

  if (is_meta) {
    # plot setup for metafor-objects
    p <- ggplot(x, aes(x = .data$Parameter, y = .data$Coefficient, color = .data$group)) +
      geom_hline(aes(yintercept = y_intercept), linetype = "dotted") +
      geom_pointrange(aes(ymin = .data$CI_low, ymax = .data$CI_high), size = point_size, fatten = x$point_size, shape = x$shape) +
      coord_flip() +
      theme_modern(legend.position = "none") +
      scale_color_material() +
      guides(color = FALSE, size = FALSE, shape = FALSE)
  } else if (sum(grepl("^CI_low", colnames(x))) > 1) {
    # plot setup for model parameters with multiple CIs
    x <- bayestestR::reshape_ci(x)
    x$CI <- as.character(x$CI)
    p <- ggplot(x, aes(x = .data$Parameter, y = .data$Coefficient, color = .data$CI)) +
      geom_hline(aes(yintercept = y_intercept), linetype = "dotted") +
      geom_pointrange(
        aes(ymin = .data$CI_low, ymax = .data$CI_high),
        size = point_size,
        position = position_dodge(1 / length(unique(x$CI)))
      ) +
      coord_flip() +
      theme_modern() +
      scale_color_material()
  } else {
    # plot setup for regular model parameters
    x$group <- as.factor(x$Coefficient < y_intercept)
    p <- ggplot(x, aes(x = .data$Parameter, y = .data$Coefficient, color = .data$group)) +
      geom_hline(aes(yintercept = y_intercept), linetype = "dotted") +
      geom_pointrange(aes(ymin = .data$CI_low, ymax = .data$CI_high), size = point_size) +
      coord_flip() +
      theme_modern(legend.position = "none") +
      scale_color_material()
  }


  if (!is.null(pretty_names)) {
    p <- p + scale_x_discrete(labels = pretty_names)
  }

  # check for exponentiated estimates. in such cases, we transform the y-axis
  # to log-scale, to get proper proportion of exponentiated estimates. to
  # do this, we create a pretty range of values, and then look for lowest and
  # largest data points that are within this range. Thereby we have the pretty
  # values we can use as breaks and labels for the scale...
  if (exponentiated_coefs) {
    range <- 2^c(-24:16)
    x_low <- which.min(min(x$CI_low) > range) - 1
    x_high <- which.max(max(x$CI_high) < range)
    p <- p + scale_y_log10(
      breaks = range[x_low:x_high],
      limits = c(range[x_low], range[x_high]),
      labels = sprintf("%g", range[x_low:x_high])
    )
  }

  # wrap plot into facets, depending on the components
  if (is.null(n_columns)) n_columns <- ifelse(sum(has_component, has_response, has_effects) > 1, 2, 1)

  if (ordinal_model)
    facet_scales <- "free_y"
  else
    facet_scales <- "free"

  if (has_component && has_response && has_effects) {
    p <- p + facet_wrap(~Response + Effects + Component, ncol = n_columns, scales = facet_scales)
  } else if (has_component && has_effects) {
    p <- p + facet_wrap(~Effects + Component, ncol = n_columns, scales = facet_scales)
  } else if (has_component && has_response) {
    p <- p + facet_wrap(~Response + Component, ncol = n_columns, scales = facet_scales)
  } else if (has_effects && has_response) {
    p <- p + facet_wrap(~Response + Effects , ncol = n_columns, scales = facet_scales)
  } else if (has_component) {
    p <- p + facet_wrap(~Component, ncol = n_columns, scales = facet_scales)
  } else if (has_effects) {
    p <- p + facet_wrap(~Effects, ncol = n_columns, scales = facet_scales)
  } else if (has_response) {
    p <- p + facet_wrap(~Response, ncol = n_columns, scales = facet_scales)
  } else if (has_subgroups) {
    suppressWarnings(p <- p + facet_grid(Subgroup~., scales = "free", space = "free"))
  }

  p + labs(
    x = "Parameter",
    y = ifelse(exponentiated_coefs, "Exp(Estimate)", "Estimate"),
    colour = "CI"
  )
}