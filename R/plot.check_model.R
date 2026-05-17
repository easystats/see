#' Plot method for checking model assumptions
#'
#' The `plot()` method for the `performance::check_model()` function.
#' Diagnostic plots for regression models.
#'
#' @inheritParams print.see_performance_pp_check
#' @inheritParams data_plot
#' @inheritParams plots
#'
#' @return A ggplot2-object.
#'
#' @seealso See also the vignette about
#'   [`check_model()`](https://easystats.github.io/performance/articles/check_model.html).
#'
#' @details
#' Larger models (with many observations) may take a longer time to render.
#' Thus, the number of data points is limited to 2000 by default. Use
#' `plot(check_model(), maximum_dots = <number>)` (or
#' `check_model(maximum_dots = <number>)`) to define the number of data points
#' that should be shown in the plots.
#'
#' @examplesIf require("patchwork") && FALSE
#' library(performance)
#'
#' model <- lm(qsec ~ drat + wt, data = mtcars)
#' plot(check_model(model))
#'
#' @export
plot.see_check_model <- function(
  x,
  theme = NULL,
  colors = NULL,
  type = c("density", "discrete_dots", "discrete_interval", "discrete_both"),
  n_columns = 2,
  ...
) {
  # Initialize an empty list to store the individual ggplot objects
  p <- list()
  dots <- list(...)

  # 1. Extract arguments and settings ------------------------------------------
  # Read graphical parameters and model information stored as attributes in 'x'
  panel <- .default_value(x, "panel", TRUE)
  check <- .default_value(x, "check", "all")
  size_point <- .default_value(x, "dot_size", 2)
  linewidth <- .default_value(x, "line_size", 0.8)
  show_labels <- .default_value(x, "show_labels", TRUE)
  size_text <- .default_value(x, "text_size")
  base_size <- .default_value(x, "base_size", 10)
  size_axis_title <- .default_value(x, "axis_title_size", base_size)
  size_title <- .default_value(x, "title_size", 12)
  alpha_level <- .default_value(x, "alpha", 0.2)
  alpha_dot <- .default_value(x, "alpha_dot", 0.8)
  show_dots <- .default_value(x, "show_dots", TRUE)

  # Check for Confidence Intervals: Backwards compatibility for older package
  # versions
  show_ci <- !isFALSE(attr(x, "show_ci"))

  detrend <- .default_value(x, "detrend", TRUE)
  model_info <- .default_value(x, "model_info")
  overdisp_type <- attr(x, "overdisp_type")
  plot_type <- attr(x, "type")
  model_class <- attr(x, "model_class")
  max_dots <- .default_value(x, "maximum_dots")

  # Override maximum dots if provided directly via '...'
  if (is.null(max_dots) && !is.null(dots$maximum_dots)) {
    max_dots <- dots$maximum_dots
  }

  # Resolve the 'type' argument based on inputs and defaults
  if (
    missing(type) &&
      !is.null(plot_type) &&
      plot_type %in%
        c("density", "discrete_dots", "discrete_interval", "discrete_both")
  ) {
    type <- plot_type
  } else {
    type <- match.arg(type)
  }

  # 2. Set default values ------------------------------------------------------
  theme <- .set_default_theme(
    x,
    theme,
    base_size,
    size_axis_title,
    size_title
  )

  if (is.null(colors)) {
    colors <- .default_value(x, "colors", c("#3aaf85", "#1b6ca8", "#cd201f"))
  }
  colors <- unname(colors)

  # 3. Build plot panels -------------------------------------------------------

  # Define common arguments for plot functions to ensure consistency
  common_args <- list(
    theme = theme,
    base_size = base_size,
    size_title = size_title,
    size_axis_title = size_axis_title,
    size_point = size_point,
    linewidth = linewidth
  )

  # Each block below checks if the specific diagnostic test is requested in 'check'
  # and if the corresponding data exists in 'x'. If so, it generates the plot.

  # Posterior Predictive Check
  if (.should_plot(x, check, "PP_CHECK", "pp_check")) {
    x$NORM <- NULL # Prevent duplicate normality plotting if PP_CHECK is used
    fun_args <- c(
      list(x$PP_CHECK),
      common_args,
      list(
        type = type,
        check_model = TRUE,
        adjust_legend = TRUE,
        colors = colors[1:2]
      )
    )
    p$PP_CHECK <- do.call(plot.see_performance_pp_check, fun_args)
  }

  # Non-Constant Error Variance (Linearity/Homoscedasticity)
  if (.should_plot(x, check, "NCV", c("ncv", "linearity"))) {
    fun_args <- c(
      list(x$NCV),
      common_args,
      list(
        alpha_level = alpha_level,
        colors = colors,
        alpha_dot = alpha_dot,
        show_dots = show_dots,
        show_ci = show_ci,
        maximum_dots = max_dots
      )
    )
    p$NCV <- do.call(.plot_diag_linearity, fun_args)
  }

  # Binned Residuals
  if (.should_plot(x, check, "BINNED_RESID", "binned_residuals")) {
    x$HOMOGENEITY <- NULL # Prevent conflict with standard homogeneity plot
    fun_args <- c(
      list(x$BINNED_RESID),
      common_args,
      list(
        colors = colors[c(2, 3, 1)],
        adjust_legend = TRUE,
        check_model = TRUE,
        show_dots = show_dots
      )
    )
    p$BINNED_RESID <- do.call(plot.see_binned_residuals, fun_args)
  }

  # Overdispersion
  if (.should_plot(x, check, "OVERDISPERSION", "overdispersion")) {
    fun_args <- c(
      list(x$OVERDISPERSION),
      common_args,
      list(colors = colors[c(1, 2)], type = overdisp_type)
    )
    p$OVERDISPERSION <- do.call(.plot_diag_overdispersion, fun_args)
  }

  # Homogeneity of Variance
  if (.should_plot(x, check, "HOMOGENEITY", "homogeneity")) {
    fun_args <- c(
      list(x$HOMOGENEITY),
      common_args,
      list(
        alpha_level = alpha_level,
        colors = colors,
        alpha_dot = alpha_dot,
        show_dots = show_dots,
        show_ci = show_ci,
        maximum_dots = max_dots
      )
    )
    p$HOMOGENEITY <- do.call(.plot_diag_homogeneity, fun_args)
  }

  # Influential Observations (Outliers)
  if (.should_plot(x, check, "INFLUENTIAL", c("outliers", "influential"))) {
    fun_args <- c(
      list(x$INFLUENTIAL),
      common_args,
      list(
        show_labels = show_labels,
        size_text = size_text,
        colors = colors,
        alpha_dot = alpha_dot,
        show_dots = show_dots,
        maximum_dots = max_dots
      )
    )
    p$OUTLIERS <- do.call(.plot_diag_outliers_dots, fun_args)
  }

  # Variance Inflation Factor (Multicollinearity)
  if (.should_plot(x, check, "VIF", "vif")) {
    fun_args <- c(
      list(x$VIF),
      common_args,
      list(
        colors = colors,
        ci_data = attributes(x$VIF)$CI,
        is_check_model = TRUE
      )
    )
    fun_args$size_point <- 1.5 * fun_args$size_point
    p$VIF <- do.call(.plot_diag_vif, fun_args)
  }

  # Quantile-Quantile (QQ) Plot for Residuals
  if (.should_plot(x, check, "QQ", "qq")) {
    fun_args <- c(
      list(x$QQ),
      common_args,
      list(
        alpha_dot = alpha_dot,
        colors = colors,
        detrend = detrend
      )
    )
    # Check if object is from simulated residuals (e.g., DHARMa)
    if (inherits(x$QQ, "performance_simres")) {
      fun_args$size_point <- 0.9 * fun_args$size_point
      fun_args$alpha <- alpha_level
      p$QQ <- do.call(plot, fun_args)
    } else {
      fun_args <- c(
        fun_args,
        list(
          alpha_level = alpha_level,
          show_dots = TRUE, # qq-plots w/o dots makes no sense
          model_info = model_info,
          model_class = model_class,
          maximum_dots = max_dots
        )
      )
      p$QQ <- do.call(.plot_diag_qq, fun_args)
    }
  }

  # Normality of Residuals
  if (.should_plot(x, check, "NORM", "normality")) {
    fun_args <- c(
      list(x$NORM),
      common_args,
      list(alpha_level = alpha_level, colors = colors)
    )
    p$NORM <- do.call(.plot_diag_norm, fun_args)
  }

  # Random Effects QQ Plot
  if (.should_plot(x, check, "REQQ", "reqq")) {
    fun_args <- c(
      list(x$REQQ),
      common_args,
      list(
        alpha_level = alpha_level,
        colors = colors,
        alpha_dot = alpha_dot,
        show_dots = TRUE, # qq-plots w/o dots makes no sense
        maximum_dots = max_dots
      )
    )
    ps <- do.call(.plot_diag_reqq, fun_args)
    # Append all random effects plots to the main list
    for (i in seq_along(ps)) {
      p[[length(p) + 1]] <- ps[[i]]
    }
  }

  # 4. Finalizing and returning the output -------------------------------------
  # If requested, combine into a single patchwork grid
  if (panel) {
    pw <- plots(p, n_columns = n_columns)
    .safe_print_plots(pw, ...)
    invisible(pw)
  } else {
    p
  }
}


.should_plot <- function(x, check, component, triggers) {
  component %in%
    names(x) &&
    !is.null(x[[component]]) &&
    any(c(triggers, "all") %in% check)
}


# Helper function to plot linearity diagnostic
.plot_diag_linearity <- function(
  x,
  size_point,
  linewidth,
  size_axis_title = 10,
  size_title = 12,
  alpha_level = 0.2,
  theme = NULL,
  base_size = 10,
  colors = unname(social_colors(c("green", "blue", "red"))),
  alpha_dot = 0.8,
  show_dots = TRUE,
  show_ci = TRUE,
  maximum_dots = 2000,
  ...
) {
  # Standardize the theme
  theme <- .set_default_theme(
    x,
    theme,
    base_size,
    size_axis_title,
    size_title,
    default_theme = ggplot2::theme_grey()
  )

  # Downsample data if it exceeds the maximum dots threshold (for rendering
  # performance) (issue #420)
  x <- .sample_for_plot(x, maximum_dots = maximum_dots, ...)

  # Base plot initialization
  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, y = .data$y))

  # Conditionally add raw data points
  if (isTRUE(show_dots)) {
    p <- p +
      geom_point2(
        colour = colors[2],
        size = size_point,
        alpha = alpha_dot
      )
  }

  # Add smoother, reference line, labels, and theme
  p +
    ggplot2::geom_smooth(
      method = "loess",
      se = show_ci,
      formula = y ~ x,
      alpha = alpha_level,
      linewidth = linewidth,
      colour = colors[1]
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Linearity",
      subtitle = "Reference line should be flat and horizontal"
    ) +
    theme
}
