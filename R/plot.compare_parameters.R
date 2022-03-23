#' Plot method for comparison of model parameters
#'
#' The `plot()` method for the `parameters::compare_parameters()`
#' function.
#'
#' @param dodge_position Numeric value specifying the amount of "dodging"
#'   (spacing) between geoms.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_bayesfactor_models
#' @inheritParams plot.see_parameters_model
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_parameters_brms_meta
#'
#' @return A ggplot2-object.
#'
#' @examples
#' if (require("insight") &&
#'   require("parameters") &&
#'   packageVersion("insight") >= "0.13.0") {
#'   data(iris)
#'   lm1 <- lm(Sepal.Length ~ Species, data = iris)
#'   lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#'   lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#'   result <- compare_parameters(lm1, lm2, lm3)
#'   plot(result)
#' }
#' @export
plot.see_compare_parameters <- function(x,
                                        show_intercept = FALSE,
                                        size_point = .8,
                                        size_text = NA,
                                        dodge_position = .8,
                                        sort = NULL,
                                        n_columns = NULL,
                                        show_labels = FALSE,
                                        ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  # retrieve settings ----------------

  # is exp?
  exponentiated_coefs <- isTRUE(attributes(x)$exponentiate)
  y_intercept <- ifelse(exponentiated_coefs, 1, 0)

  # add coefficients and CIs?
  add_values <- isTRUE(show_labels)

  # ordinal model? needed for free facet scales later...
  ordinal_model <- isTRUE(attributes(x)$ordinal_model)

  # create text string for estimate and CI
  x$Estimate_CI <- trimws(sprintf(
    "%.2f %s",
    x$Coefficient,
    insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = 2, zap_small = TRUE)
  ))
  x$Estimate_CI[x$Estimate_CI == "NA"] <- ""

  if ("Subgroup" %in% colnames(x)) {
    x$Subgroup <- factor(x$Subgroup, levels = unique(x$Subgroup))
  }

  x <- .fix_facet_names(x)

  # remember components
  has_effects <- "Effects" %in% colnames(x) && length(unique(x$Effects)) > 1
  has_component <- "Component" %in% colnames(x) && length(unique(x$Component)) > 1
  has_response <- "Response" %in% colnames(x) && length(unique(x$Response)) > 1
  has_subgroups <- "Subgroup" %in% colnames(x) && length(unique(x$Subgroup)) > 1

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

  p <- ggplot(x, aes(y = .data$Parameter, x = .data$Coefficient, color = .data$group)) +
    geom_vline(aes(xintercept = y_intercept), linetype = "dotted") +
    geom_pointrange(aes(xmin = .data$CI_low, xmax = .data$CI_high), size = size_point, position = position_dodge(dodge_position)) +
    theme_modern() +
    scale_color_material()

  # add coefficients and CIs?
  if (add_values) {
    # add some space to the right panel for text
    space_factor <- sqrt(ceiling(diff(c(min(x$CI_low, na.rm = TRUE), max(x$CI_high, na.rm = TRUE)))) / 5)
    new_range <- pretty(c(min(x$CI_low, na.rm = TRUE), max(x$CI_high, na.rm = TRUE) + space_factor))

    p <- p +
      geom_text(
        mapping = aes(label = .data$Estimate_CI, y = Inf),
        colour = "black", hjust = "inward", size = size_text,
        position = position_dodge2(dodge_position)
      ) +
      xlim(c(min(new_range), max(new_range)))
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
    if (add_values) {
      # add some space to the right panel for text
      new_range <- pretty(2 * max(x$CI_high))
      x_high <- which.max(max(new_range) < range)
    }
    p <- p + scale_x_continuous(
      trans = "log",
      breaks = range[x_low:x_high],
      limits = c(range[x_low], range[x_high]),
      labels = sprintf("%g", range[x_low:x_high])
    )
  }

  # wrap plot into facets, depending on the components
  if (is.null(n_columns)) n_columns <- ifelse(sum(has_component, has_response, has_effects) > 1, 2, 1)

  if (ordinal_model) {
    facet_scales <- "free_x"
  } else {
    facet_scales <- "free"
  }

  axis_title_in_facet <- FALSE

  if (has_component && has_response && has_effects) {
    p <- p + facet_wrap(~ Response + Effects + Component, ncol = n_columns, scales = facet_scales)
  } else if (has_component && has_effects) {
    p <- p + facet_wrap(~ Effects + Component, ncol = n_columns, scales = facet_scales)
  } else if (has_component && has_response) {
    p <- p + facet_wrap(~ Response + Component, ncol = n_columns, scales = facet_scales)
  } else if (has_effects && has_response) {
    p <- p + facet_wrap(~ Response + Effects, ncol = n_columns, scales = facet_scales)
  } else if (has_component) {
    p <- p + facet_wrap(~Component, ncol = n_columns, scales = facet_scales)
  } else if (has_effects) {
    p <- p + facet_wrap(~Effects, ncol = n_columns, scales = facet_scales)
  } else if (has_response) {
    p <- p + facet_wrap(~Response, ncol = n_columns, scales = facet_scales)
  } else if (has_subgroups) {
    suppressWarnings(p <- p + facet_grid(Subgroup ~ ., scales = "free", space = "free"))
  }

  if (isTRUE(axis_title_in_facet)) {
    p + labs(
      y = "Parameter",
      x = NULL,
      colour = "Model"
    )
  } else {
    p + labs(
      y = "Parameter",
      x = "Estimate",
      colour = "Model"
    )
  }
}




#' @export
data_plot.see_compare_parameters <- function(x, ...) {
  col_coefficient <- which(grepl("^(Coefficient|Log-Odds|Log-Mean|Odds Ratio|Risk Ratio|IRR)\\.", colnames(x)))
  col_ci_low <- which(grepl("^CI_low\\.", colnames(x)))
  col_ci_high <- which(grepl("^CI_high\\.", colnames(x)))
  col_p <- which(grepl("^p\\.", colnames(x)))

  out1 <- .reshape_to_long(x, values_to = "Coefficient", columns = colnames(x)[col_coefficient])[c("Parameter", "Component", "group", "Coefficient")]
  out2 <- .reshape_to_long(x, values_to = "CI_low", columns = colnames(x)[col_ci_low])["CI_low"]
  out3 <- .reshape_to_long(x, values_to = "CI_high", columns = colnames(x)[col_ci_high])["CI_high"]
  out4 <- .reshape_to_long(x, values_to = "p", columns = colnames(x)[col_p])["p"]

  dataplot <- cbind(out1, out2, out3, out4)
  dataplot$group <- gsub("(.*)\\.(.*)", "\\2", dataplot$group)

  rownames(dataplot) <- NULL

  exp_coef <- unique(unlist(insight::compact_list(lapply(x, function(i) {
    attributes(i)$exponentiate
  }))))
  attr(dataplot, "exponentiate") <- !is.null(exp_coef) && any(exp_coef != FALSE)

  class(dataplot) <- c("data_plot", "see_compare_parameters", class(dataplot))
  dataplot
}
