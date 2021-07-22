#' Plot method for model parameters
#'
#' The `plot()` method for the `parameters::model_parameters()` function.
#'
#' @param type Character indicating the type of plot. Only applies for model
#'   parameters from meta-analysis objects (e.g. \pkg{metafor}).
#' @param component Character indicating which component of the model should be
#'   plotted.
#' @param weight_points Logical. If `TRUE`, for meta-analysis objects, point
#'   size will be adjusted according to the study-weights.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_bayesfactor_models
#' @inheritParams plot.see_cluster_analysis
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_parameters_brms_meta
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(parameters)
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- model_parameters(m)
#' result
#' plot(result)
#' @export
plot.see_parameters_model <- function(x,
                                      show_intercept = FALSE,
                                      size_point = .8,
                                      size_text = NULL,
                                      sort = NULL,
                                      n_columns = NULL,
                                      type = c("forest", "funnel"),
                                      weight_points = TRUE,
                                      ...) {
  if (!any(grepl("Coefficient", colnames(x), fixed = TRUE))) {
    colnames(x)[which.min(match(colnames(x), c("Median", "Mean", "Map")))] <- "Coefficient"
  }

  # retrieve settings ----------------

  # is exp?
  exponentiated_coefs <- isTRUE(attributes(x)$exponentiate)
  y_intercept <- ifelse(exponentiated_coefs, 1, 0)

  # label for coefficient scale
  coefficient_name <- attributes(x)$coefficient_name
  zi_coefficient_name <- attributes(x)$zi_coefficient_name

  # add coefficients and CIs?
  add_values <- !is.null(size_text) && !is.na(size_text)

  # ordinal model? needed for free facet scales later...
  ordinal_model <- isTRUE(attributes(x)$ordinal_model)

  # brms has some special handling...
  is_brms <- inherits(x, c("parameters_stan", "parameters_brms"))

  # check column names, differs for standardized models
  if ("Std_Coefficient" %in% colnames(x)) {
    colnames(x)[which(colnames(x) == "Std_Coefficient")] <- "Coefficient"
  }

  # create text string for estimate and CI
  x$Estimate_CI <- sprintf(
    "%.2f %s",
    x$Coefficient,
    insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = 2, zap_small = TRUE)
  )

  # do we have a measure for meta analysis (to label axis)
  meta_measure <- attributes(x)$measure

  if (is_brms) {
    cleaned_parameters <- attributes(x)$parameter_info
    if (!is.null(cleaned_parameters)) {
      x <- merge(x, cleaned_parameters, sort = FALSE)
      x$Parameter <- x$Cleaned_Parameter
      if (all(x$Group == "")) {
        x$Group <- NULL
      } else {
        x <- x[!grepl("^SD/Cor", x$Group), , drop = FALSE]
      }
    }
  }

  if ("Subgroup" %in% colnames(x)) {
    x$Subgroup <- factor(x$Subgroup, levels = unique(x$Subgroup))
  }

  # do we have prettified names?
  pretty_names <- attributes(x)$pretty_names

  x <- .fix_facet_names(x)

  if (is_brms && "Group" %in% colnames(x)) {
    x$Effects[x$Group != ""] <- paste0(x$Effects[x$Group != ""], " (", x$Group[x$Group != ""], ")")
  }

  # remember components
  has_effects <- "Effects" %in% colnames(x) && length(unique(x$Effects)) > 1
  has_component <- "Component" %in% colnames(x) && length(unique(x$Component)) > 1
  has_response <- "Response" %in% colnames(x) && length(unique(x$Response)) > 1
  has_subgroups <- "Subgroup" %in% colnames(x) && length(unique(x$Subgroup)) > 1

  mc <- attributes(x)$model_class
  cp <- attributes(x)$cleaned_parameters
  is_meta <- !is.null(mc) && mc %in% c("rma", "rma.mv", "rma.uni", "metaplus")
  is_meta_bma <- !is.null(mc) && mc %in% c("meta_random", "meta_fixed", "meta_bma")

  # minor fixes for Bayesian models
  if (!is.null(mc) && !is.null(cp) && mc %in% c("stanreg", "stanmvreg", "brmsfit")) {
    if (length(cp) == length(x$Parameter)) {
      x$Parameter <- cp
    }
  }

  # data preparation for metafor-objects
  if (is_meta) {
    overall <- which(x$Parameter == "(Intercept)")
    if (length(overall) == 0) overall <- which(x$Parameter == "Overall")
    x$Parameter[overall] <- "Overall"
    x$group <- "study"
    x$group[overall] <- "Overall"
    if (isTRUE(weight_points)) {
      x$size_point <- sqrt(x$Weight)
      x$size_point[overall] <- 8
    } else {
      x$size_point <- 2.5
    }
    x$shape <- 19
    x$shape[overall] <- 18

    type <- match.arg(type)

    if (type == "funnel") {
      if (missing(size_point)) size_point <- 2.5
      return(.funnel_plot(x, size_point, meta_measure))
    }
  }

  # data preparation for metaBMA-objects
  if (is_meta_bma) {
    overall <- which(x$Component == "meta")
    x$group <- "study"
    x$group[overall] <- "Overall"
    x$size_point <- sqrt(x$Weight)
    x$size_point[overall] <- 8
    x$shape <- 19
    x$shape[overall] <- 18
    x$Component <- NULL
    has_component <- FALSE
  }

  # if we have a model with multiple responses or response levels
  # remove name of level from parameter name, as we split the plot
  # by response level anyway...
  if (has_response) {
    for (i in rev(sort(unique(x$Response)))) {
      x$Parameter <- gsub(i, "", x$Parameter)
      if (!is.null(pretty_names)) {
        names(pretty_names) <- gsub(i, "", names(pretty_names))
      }
    }
    x$Parameter <- gsub("^\\:(.*)", "\\1", x$Parameter)
    if (!is.null(pretty_names)) {
      names(pretty_names) <- gsub("^\\:(.*)", "\\1", names(pretty_names))
    }
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

  if (is_meta || is_meta_bma) {
    # plot setup for metafor-objects
    p <- ggplot(x, aes(y = .data$Parameter, x = .data$Coefficient, color = .data$group)) +
      geom_vline(aes(xintercept = y_intercept), linetype = "dotted") +
      geom_pointrange(aes(xmin = .data$CI_low, xmax = .data$CI_high), size = size_point, fatten = x$size_point, shape = x$shape) +
      theme_modern(legend.position = "none") +
      scale_color_material() +
      guides(color = "none", size = "none", shape = "none")
  } else if (sum(grepl("^CI_low", colnames(x))) > 1) {
    # plot setup for model parameters with multiple CIs
    x <- bayestestR::reshape_ci(x)
    x$CI <- as.character(x$CI)
    p <- ggplot(x, aes(y = .data$Parameter, x = .data$Coefficient, color = .data$CI)) +
      geom_vline(aes(xintercept = y_intercept), linetype = "dotted") +
      geom_pointrange(
        aes(xmin = .data$CI_low, xmax = .data$CI_high),
        size = size_point,
        position = position_dodge(1 / length(unique(x$CI)))
      ) +
      theme_modern() +
      scale_color_material()
  } else {
    # plot setup for regular model parameters
    x$group <- as.factor(x$Coefficient < y_intercept)
    p <- ggplot(x, aes(y = .data$Parameter, x = .data$Coefficient, color = .data$group)) +
      geom_vline(aes(xintercept = y_intercept), linetype = "dotted") +
      geom_pointrange(aes(xmin = .data$CI_low, xmax = .data$CI_high), size = size_point) +
      theme_modern(legend.position = "none") +
      scale_color_material()
  }


  if (!is.null(pretty_names)) {
    p <- p + scale_y_discrete(labels = pretty_names)
  }

  # add coefficients and CIs?
  if (add_values) {
    # add some space to the right panel for text
    space_factor <- sqrt(ceiling(diff(c(min(x$CI_low), max(x$CI_high)))) / 5)
    new_range <- pretty(c(min(x$CI_low), max(x$CI_high) + space_factor))

    p <- p +
      geom_text(
        mapping = aes(label = .data$Estimate_CI, y = Inf),
        colour = "black", hjust = "inward", size = size_text
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
    if (!is.null(zi_coefficient_name) && !is.null(coefficient_name) && zi_coefficient_name != coefficient_name) {
      coef_labeller <- function(string) {
        paste0(gsub("(\\(|\\))", "", string), " (", c(coefficient_name, zi_coefficient_name), ")")
      }
      p <- p + facet_wrap(~Component, ncol = n_columns, scales = facet_scales, labeller = as_labeller(coef_labeller))
      axis_title_in_facet <- TRUE
    } else {
      p <- p + facet_wrap(~Component, ncol = n_columns, scales = facet_scales)
    }
  } else if (has_effects) {
    p <- p + facet_wrap(~Effects, ncol = n_columns, scales = facet_scales)
  } else if (has_response) {
    p <- p + facet_wrap(~Response, ncol = n_columns, scales = facet_scales)
  } else if (has_subgroups) {
    suppressWarnings(p <- p + facet_grid(Subgroup ~ ., scales = "free", space = "free"))
  }

  if (isTRUE(is_meta)) {
    measure <- .meta_measure(meta_measure)
    p + labs(
      y = "",
      x = measure,
      colour = "CI"
    )
  } else {
    if (isTRUE(axis_title_in_facet)) {
      p + labs(
        y = "Parameter",
        x = NULL,
        colour = "CI"
      )
    } else {
      p + labs(
        y = "Parameter",
        x = ifelse(is.null(coefficient_name), ifelse(exponentiated_coefs, "Exp(Estimate)", "Estimate"), coefficient_name),
        colour = "CI"
      )
    }
  }
}



.funnel_plot <- function(x, size_point = 3, meta_measure = NULL) {
  max_y <- max(pretty(max(x$SE) * 105)) / 100
  measure <- .meta_measure(meta_measure)

  dat_funnel <- data.frame(
    se_range = effectsize::change_scale(1:(nrow(x) * 10), to = c(0, max_y))
  )
  estimate <- x$Coefficient[x$Parameter == "Overall"]

  dat_funnel$ci_low <- estimate - stats::qnorm(.975) * dat_funnel$se_range
  dat_funnel$ci_high <- estimate + stats::qnorm(.975) * dat_funnel$se_range

  d_polygon <- data.frame(
    x = c(min(dat_funnel$ci_low), estimate, max(dat_funnel$ci_high)),
    y = c(max(dat_funnel$se_range), 0, max(dat_funnel$se_range))
  )

  ggplot(x, aes(x = .data$Coefficient, y = .data$SE)) +
    scale_y_reverse(expand = c(0, 0), limits = c(max_y, 0)) +
    geom_polygon(data = d_polygon, aes(.data$x, .data$y), fill = "grey80", alpha = .3) +
    geom_line(data = dat_funnel, mapping = aes(x = .data$ci_low, y = .data$se_range), linetype = "dashed", color = "grey70") +
    geom_line(data = dat_funnel, mapping = aes(x = .data$ci_high, y = .data$se_range), linetype = "dashed", color = "grey70") +
    theme_modern() +
    geom_vline(xintercept = estimate, colour = "grey70") +
    geom_point(size = size_point, colour = "#34465d") +
    labs(y = "Standard Error", x = measure)
}



.meta_measure <- function(meta_measure) {
  switch(meta_measure,
    "MD" = "Raw Mean Difference",
    "SMDH" = ,
    "SMD" = "Standardized Mean Difference",
    "ROM" = "Log transformed Ratio of Means",
    "D2ORL" = ,
    "D2ORN" = "Transformed Standardized Mean Difference",
    "UCOR" = ,
    "COR" = "Raw Correlation Coefficient",
    "ZCOR" = "Z transformed Correlation Coefficient",
    "PHI" = "Phi Coefficient",
    "RR" = "Log Risk Ratio",
    "OR" = "Log Odds Ratio",
    "RD" = "Risk Difference",
    "AS" = "Root transformed Risk Difference",
    "PETO" = "Peto's Log Odds Ratio",
    "PBIT" = "Standardized Mean Difference (Probit-transformed)",
    "OR2DL" = ,
    "OR2DN" = "Standardized Mean Difference (Odds Ratio-transformed)",
    "IRR" = "Log Incidence Rate Ratio",
    "IRD" = "Incidence Rate Difference",
    "IRSD" = "Square Root transformed Incidence Rate Difference",
    "GEN" = "Generic Estimate",
    "Estimate"
  )
}
