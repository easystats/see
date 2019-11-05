#' @importFrom bayestestR reshape_ci
#' @param sort If \code{NULL}, coefficients are plotted in the order as they appear in the summary. Use \code{sort = "ascending"} (or \code{sort = TRUE})) resp. \code{sort = "descending"} to sort coefficients in ascending or descending order.
#' @rdname data_plot
#' @export
plot.see_parameters_model <- function(x, show_intercept = FALSE, point_size = .8, sort = NULL, n_columns = NULL, ...) {
  if (!any(grepl("Coefficient", colnames(x), fixed = TRUE))) {
    colnames(x)[which.min(match(colnames(x), c("Median", "Mean", "Map")))] <- "Coefficient"
  }

  # remember components

  has_effects <- "Effects" %in% colnames(x) && length(unique(x$Effects)) > 1
  has_component <- "Component" %in% colnames(x) && length(unique(x$Component)) > 1
  has_response <- "Response" %in% colnames(x) && length(unique(x$Response)) > 1

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
    }
    x$Parameter <- gsub("^\\:(.*)", "\\1", x$Parameter)
  }


  # make sure components are sorted in original order, not alphabetically

  if (has_effects) {
    x$Effects <- factor(x$Effects, levels = unique(x$Effects))
  }
  if (has_component) {
    x$Component <- factor(x$Component, levels = unique(x$Component))
  }


  ## TODO check for brms models, "Intercept" may be named differently
  if (!show_intercept) x <- x[x$Parameter != "(Intercept)", ]

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
      geom_hline(aes(yintercept = 0), linetype = "dotted") +
      geom_pointrange(aes(ymin = .data$CI_low, ymax = .data$CI_high), size = point_size, fatten = x$point_size, shape = x$shape) +
      coord_flip() +
      theme_modern(legend.position = "none") +
      scale_color_material() +
      guides(color = FALSE, size = FALSE, shape = FALSE)
  }
  else if (sum(grepl("^CI_low", colnames(x))) > 1) {

    # plot setup for model parameters with multiple CIs

    x <- bayestestR::reshape_ci(x)
    x$CI <- as.character(x$CI)
    p <- ggplot(x, aes(x = .data$Parameter, y = .data$Coefficient, color = .data$CI)) +
      geom_hline(aes(yintercept = 0), linetype = "dotted") +
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

    x$group <- as.factor(x$Coefficient < 0)
    p <- ggplot(x, aes(x = .data$Parameter, y = .data$Coefficient, color = .data$group)) +
      geom_hline(aes(yintercept = 0), linetype = "dotted") +
      geom_pointrange(aes(ymin = .data$CI_low, ymax = .data$CI_high), size = point_size) +
      coord_flip() +
      theme_modern(legend.position = "none") +
      scale_color_material()
  }


  # wrap plot into facets, depending on the components

  if (is.null(n_columns)) n_columns <- ifelse(sum(has_component, has_response, has_effects) > 1, 2, 1)

  if (has_component && has_response && has_effects) {
    p <- p + facet_wrap(~Response + Effects + Component, ncol = n_columns, scales = "free")
  } else if (has_component && has_effects) {
    p <- p + facet_wrap(~Effects + Component, ncol = n_columns, scales = "free")
  } else if (has_component && has_response) {
    p <- p + facet_wrap(~Response + Component, ncol = n_columns, scales = "free")
  } else if (has_effects && has_response) {
    p <- p + facet_wrap(~Response + Effects , ncol = n_columns, scales = "free")
  } else if (has_component) {
    p <- p + facet_wrap(~Component, ncol = n_columns, scales = "free")
  } else if (has_effects) {
    p <- p + facet_wrap(~Effects, ncol = n_columns, scales = "free")
  } else if (has_response) {
    p <- p + facet_wrap(~Response, ncol = n_columns, scales = "free")
  }

  p + labs(x = "Estimate", y = "Parameter", colour = "CI")
}