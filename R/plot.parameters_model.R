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
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_check_outliers
#' @inheritParams plot.see_parameters_brms_meta
#' @param show_estimate Should the point estimate of each parameter be shown?
#'   (default: `TRUE`)
#' @param show_interval Should the compatibility interval(s) of each parameter
#'   be shown? (default: `TRUE`)
#' @param show_density Should the compatibility density (i.e., posterior,
#'   bootstrap, or confidence density) of each parameter be shown?
#'   (default: `FALSE`)
#' @param log_scale Should exponentiated coefficients (e.g., odds-ratios) be
#'   plotted on a log scale? (default: `FALSE`)
#' @param n_columns For models with multiple components (like fixed and random,
#'   count and zero-inflated), defines the number of columns for the
#'   panel-layout. If `NULL`, a single, integrated plot is shown.
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
                                      size_point = 0.8,
                                      size_text = NA,
                                      sort = NULL,
                                      n_columns = NULL,
                                      type = c("forest", "funnel"),
                                      weight_points = TRUE,
                                      show_labels = FALSE,
                                      show_estimate = TRUE,
                                      show_interval = TRUE,
                                      show_density = FALSE,
                                      log_scale = FALSE,
                                      ...) {
  # retrieve settings ----------------
  model_attributes <- attributes(x)[!names(attributes(x)) %in% c("names", "row.names", "class")]

  # show intercept for intercept-only models
  if (insight::is_model(x) && insight::is_nullmodel(x)) {
    show_intercept <- TRUE
  }

  # Clean up column names
  if (!any(grepl("Coefficient", colnames(x), fixed = TRUE))) {
    colnames(x)[which.min(match(colnames(x), c("Median", "Mean", "Map", "MAP", model_attributes$coefficient_name)))] <- "Coefficient"
  }

  if (!any(grepl("Parameter", colnames(x), fixed = TRUE))) {
    if (length(model_attributes$parameter_names) > 1L) {
      collapsed_params <- apply(
        do.call(
          cbind,
          lapply(
            model_attributes$parameter_names,
            function(param) paste(param, "=", x[[param]])
          )
        ), 1,
        paste,
        collapse = ", "
      )
      x$Parameter <- collapsed_params
    } else {
      colnames(x)[which.min(match(colnames(x), model_attributes$parameter_names))] <- "Parameter"
    }
  }

  # is exp?
  exponentiated_coefs <- isTRUE(model_attributes$exponentiate)
  y_intercept <- as.numeric(exponentiated_coefs)

  # label for coefficient scale
  coefficient_name <- model_attributes$coefficient_name
  zi_coefficient_name <- model_attributes$zi_coefficient_name

  # add coefficients and CIs?
  add_values <- isTRUE(show_labels)

  # ordinal model? needed for free facet scales later...
  ordinal_model <- isTRUE(model_attributes$ordinal_model)

  # bootstrap models handle densities differently
  is_bootstrap <- isTRUE(model_attributes$bootstrap)

  # bayesian (namely brms) has some special handling...
  is_bayesian <- inherits(x, c("parameters_stan", "parameters_brms"))

  # check column names, differs for standardized models
  if ("Std_Coefficient" %in% colnames(x)) {
    colnames(x)[which(colnames(x) == "Std_Coefficient")] <- "Coefficient"
  }

  # check if multiple CIs
  if (sum(startsWith(colnames(x), "CI_low")) > 1L) {
    multiple_ci <- TRUE
    x <- datawizard::reshape_ci(x)
  } else {
    multiple_ci <- FALSE
  }

  # create text string for estimate and CI
  x$Estimate_CI <- sprintf(
    "%.2f %s",
    x$Coefficient,
    insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = 2, zap_small = TRUE)
  )

  # do we have a measure for meta analysis (to label axis)
  meta_measure <- model_attributes$measure

  if (is_bayesian) {
    cleaned_parameters <- model_attributes$parameter_info
    if (!is.null(cleaned_parameters)) {
      x <- merge(x, cleaned_parameters, sort = FALSE)
      x$Parameter <- x$Cleaned_Parameter
      if (all(x$Group == "")) {
        x$Group <- NULL
      } else {
        x <- x[!startsWith(x$Group, "SD/Cor"), , drop = FALSE]
      }
    }
    attributes(x) <- c(attributes(x), model_attributes)
  }

  if ("Subgroup" %in% colnames(x)) {
    x$Subgroup <- factor(x$Subgroup, levels = unique(x$Subgroup))
  }

  # do we have prettified names?
  pretty_names <- model_attributes$pretty_names

  x <- .fix_facet_names(x)

  if (is_bayesian && "Group" %in% colnames(x)) {
    x$Effects[x$Group != ""] <- paste0(x$Effects[x$Group != ""], " (", x$Group[x$Group != ""], ")")
  }

  # remember components
  has_effects <- "Effects" %in% colnames(x) && length(unique(x$Effects)) > 1L
  has_component <- "Component" %in% colnames(x) && length(unique(x$Component)) > 1L
  has_response <- "Response" %in% colnames(x) && length(unique(x$Response)) > 1L
  has_subgroups <- "Subgroup" %in% colnames(x) && length(unique(x$Subgroup)) > 1L

  mc <- model_attributes$model_class
  cp <- model_attributes$cleaned_parameters
  is_linear <- model_attributes$linear_model
  is_meta <- !is.null(mc) && any(mc %in% c("rma", "rma.mv", "rma.uni", "metaplus"))
  is_meta_bma <- !is.null(mc) && any(mc %in% c("meta_random", "meta_fixed", "meta_bma"))

  # minor fixes for Bayesian models
  if (!is.null(mc) && !is.null(cp) && any(mc %in% c("stanreg", "stanmvreg", "brmsfit")) && length(cp) == length(x$Parameter)) {
    x$Parameter <- cp
  }

  if (isTRUE(show_density)) {
    insight::check_if_installed("ggdist")

    # TODO: Handle Effects and Components
    # TODO: Handle meta-analysis models

    if (is_bootstrap || isTRUE(is_bayesian)) {
      if (is_bootstrap) {
        data <- model_attributes$boot_samples
      } else {
        data <- as.data.frame(.retrieve_data(x))
      }

      # MCMC or bootstrapped models
      if (is.null(data)) {
        insight::format_error("Could not retrieve parameter simulations.")
      }

      data <- datawizard::reshape_longer(
        data,
        names_to = "Parameter",
        rows_to = "Iteration",
        values_to = "Coefficient"
      )
      group <- x[, "Parameter", drop = FALSE]
      group$group <- factor(x$Coefficient < y_intercept, levels = c(FALSE, TRUE))
      data <- merge(data, group, by = "Parameter")
      if (isTRUE(exponentiated_coefs)) {
        data$Coefficient <- exp(data$Coefficient)
      }

      density_layer <- ggdist::stat_slab(
        ggplot2::aes(fill = ggplot2::after_scale(.data$color)),
        size = NA, alpha = 0.2,
        data = data
      )
    } else if (isTRUE(exponentiated_coefs)) {
      density_layer <- ggdist::stat_dist_slab(
        ggplot2::aes(
          dist = "lnorm",
          arg1 = log(.data$Coefficient),
          arg2 = .data$SE / .data$Coefficient,
          fill = ggplot2::after_scale(.data$color)
        ),
        size = NA, alpha = 0.2,
        data = function(x) x[x$CI == x$CI[1], ]
      )
    } else if (model_attributes$test_statistic == "t-statistic") {
      # t-distribution confidence densities
      density_layer <- ggdist::stat_dist_slab(
        ggplot2::aes(
          dist = "student_t",
          arg1 = .data$df_error,
          arg2 = .data$Coefficient,
          arg3 = .data$SE,
          fill = ggplot2::after_scale(.data$color)
        ),
        size = NA, alpha = 0.2,
        data = function(x) x[x$CI == x$CI[1], ]
      )
    } else {
      # normal-approximation confidence densities
      density_layer <- ggdist::stat_dist_slab(
        ggplot2::aes(
          dist = "norm",
          arg1 = .data$Coefficient,
          arg2 = .data$SE,
          fill = ggplot2::after_scale(.data$color)
        ),
        size = NA, alpha = 0.2,
        data = function(x) x[x$CI == x$CI[1], ]
      )
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


  if (!show_intercept && length(.in_intercepts(x$Parameter)) > 0L) {
    x <- x[!.in_intercepts(x$Parameter), ]
    if (show_density && (is_bayesian || is_bootstrap)) {
      data <- data[!.in_intercepts(data$Parameter), ]
      density_layer$data <- data
    }
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
    p <- ggplot2::ggplot(x, ggplot2::aes(y = .data$Parameter, x = .data$Coefficient, color = .data$group)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = y_intercept), linetype = "dotted") +
      theme_modern(legend.position = "none") +
      scale_color_material() +
      ggplot2::guides(color = "none", size = "none", shape = "none")

    if (show_density) {
      # p <- p + density_layer
      message(
        insight::format_message("Plotting densities not yet supported for meta-analysis models.")
      )
    }

    if (show_interval) {
      # TODO: Handle NA boundaries
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(xmin = .data$CI_low, xmax = .data$CI_high),
        width = 0,
        linewidth = size_point
      )
    }

    if (show_estimate) {
      p <- p + ggplot2::geom_point(size = x$size_point * size_point, shape = x$shape)
    }
  } else if (isTRUE(multiple_ci)) {
    # plot setup for model parameters with multiple CIs
    x$CI <- as.character(x$CI)

    x$group <- factor(x$Coefficient < y_intercept, levels = c(FALSE, TRUE))
    if (all(x$group == "TRUE")) {
      color_scale <- scale_color_material(reverse = TRUE)
    } else {
      color_scale <- scale_color_material()
    }

    p <- ggplot2::ggplot(x, ggplot2::aes(
      y = .data$Parameter, x = .data$Coefficient,
      size = rev(.data$CI), color = .data$group
    )) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = y_intercept), linetype = "dotted") +
      theme_modern(legend.position = "none") +
      color_scale

    if (show_density) {
      p <- p + density_layer
    }

    if (show_interval) {
      # TODO: Handle NA boundaries
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(xmin = .data$CI_low, xmax = .data$CI_high),
        width = 0
      ) +
        ggplot2::scale_size_ordinal(range = c(size_point, 3 * size_point))
    }

    if (show_estimate) {
      p <- p + ggplot2::geom_point(
        size = 4 * size_point
      )
    }
  } else {
    # plot setup for regular model parameters
    x$group <- factor(x$Coefficient < y_intercept, levels = c(FALSE, TRUE))
    if (all(x$group == "TRUE")) {
      color_scale <- scale_color_material(reverse = TRUE)
    } else {
      color_scale <- scale_color_material()
    }

    p <- ggplot2::ggplot(x, ggplot2::aes(y = .data$Parameter, x = .data$Coefficient, color = .data$group)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = y_intercept), linetype = "dotted") +
      theme_modern(legend.position = "none") +
      color_scale

    if (show_density) {
      p <- p + density_layer
    }

    if (show_interval) {
      # TODO: Handle NA boundaries
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$CI_low, xmax = .data$CI_high),
        width = 0,
        linewidth = size_point
      )
    }

    if (show_estimate) {
      if (show_density) {
        p <- p + ggplot2::geom_point(
          size = 4 * size_point,
          fill = "white",
          shape = 21
        )
      } else {
        p <- p + ggplot2::geom_point(size = 4 * size_point)
      }
    }
  }


  if (!is.null(pretty_names)) {
    p <- p + ggplot2::scale_y_discrete(labels = pretty_names)
  }

  # find min/max range based on CI
  min_ci <- min(x$CI_low, na.rm = TRUE)
  max_ci <- max(x$CI_high, na.rm = TRUE)

  # add coefficients and CIs?
  if (add_values) {
    # add some space to the right panel for text
    space_factor <- sqrt(ceiling(diff(c(min_ci, max_ci))) / 5)
    new_range <- pretty(c(min_ci, max_ci + space_factor))

    # expand scale range and add numbers to the right border
    if (!any(is.infinite(new_range)) && !anyNA(new_range)) {
      p <- p +
        geom_text(
          mapping = aes(label = .data$Estimate_CI, x = Inf),
          colour = "black", hjust = "inward", size = size_text
        ) +
        xlim(c(min(new_range), max(new_range)))
    }
  }

  # check for exponentiated estimates. in such cases,
  # if the user requests a log_scale we transform the y-axis
  # to log-scale, to get proper proportion of exponentiated estimates. to
  # do this, we create a pretty range of values, and then look for lowest and
  # largest data points that are within this range. Thereby we have the pretty
  # values we can use as breaks and labels for the scale...

  if (exponentiated_coefs && log_scale) {
    range <- 2^(-24:16)
    x_low <- which.min(min_ci > range) - 1
    x_high <- which.max(max_ci < range)

    if (add_values) {
      # add some space to the right panel for text
      new_range <- pretty(2 * max_ci)
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
    suppressWarnings(p <- p + facet_grid(Subgroup ~ ., scales = "free", space = "free")) # nolint
  }

  if (length(model_attributes$parameter_names) > 1L) {
    parameter_label <- "Parameters"
  } else {
    parameter_label <- model_attributes$parameter_names
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
        y = parameter_label,
        x = NULL,
        colour = "CI"
      )
    } else {
      p + labs(
        y = parameter_label,
        x = ifelse(is.null(coefficient_name),
          ifelse(exponentiated_coefs, "Exp(Estimate)", "Estimate"), # nolint
          coefficient_name
        ),
        colour = "CI"
      )
    }
  }
}



.funnel_plot <- function(x, size_point = 3, meta_measure = NULL) {
  max_y <- max(pretty(max(x$SE) * 105)) / 100
  measure <- .meta_measure(meta_measure)

  dat_funnel <- data.frame(
    se_range = datawizard::rescale(1:(nrow(x) * 10), to = c(0, max_y))
  )
  estimate <- x$Coefficient[x$Parameter == "Overall"]

  dat_funnel$ci_low <- estimate - stats::qnorm(0.975) * dat_funnel$se_range
  dat_funnel$ci_high <- estimate + stats::qnorm(0.975) * dat_funnel$se_range

  d_polygon <- data.frame(
    x = c(min(dat_funnel$ci_low), estimate, max(dat_funnel$ci_high)),
    y = c(max(dat_funnel$se_range), 0, max(dat_funnel$se_range))
  )

  ggplot(x, aes(x = .data$Coefficient, y = .data$SE)) +
    scale_y_reverse(expand = c(0, 0), limits = c(max_y, 0)) +
    geom_polygon(data = d_polygon, aes(.data$x, .data$y), fill = "grey80", alpha = 0.3) +
    geom_line(
      data = dat_funnel,
      mapping = aes(x = .data$ci_low, y = .data$se_range),
      linetype = "dashed",
      color = "grey70"
    ) +
    geom_line(
      data = dat_funnel,
      mapping = aes(x = .data$ci_high, y = .data$se_range),
      linetype = "dashed",
      color = "grey70"
    ) +
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
