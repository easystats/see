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
#' @seealso See also the vignette about [`check_model()`](https://easystats.github.io/performance/articles/check_model.html).
#'
#' @examplesIf require("performance") && require("patchwork")
#' model <- lm(qsec ~ drat + wt, data = mtcars)
#' plot(check_model(model))
#' @importFrom ggplot2 .data
#' @export
plot.see_check_model <- function(x,
                                 style = theme_lucid,
                                 colors = NULL,
                                 type = c("density", "discrete_dots", "discrete_interval", "discrete_both"),
                                 n_columns = 2,
                                 ...) {
  p <- list()

  # read arguments / settings from "check_model()" -----

  panel <- attr(x, "panel")
  check <- attr(x, "check")
  size_point <- attr(x, "dot_size")
  size_line <- attr(x, "line_size")
  show_labels <- attr(x, "show_labels") %||% TRUE
  size_text <- attr(x, "text_size")
  alpha_level <- attr(x, "alpha")
  dot_alpha_level <- attr(x, "dot_alpha")
  show_dots <- attr(x, "show_dots")
  detrend <- attr(x, "detrend")
  model_info <- attr(x, "model_info")
  overdisp_type <- attr(x, "overdisp_type")
  plot_type <- attr(x, "type")

  if (missing(type) && !is.null(plot_type) && plot_type %in% c("density", "discrete_dots", "discrete_interval", "discrete_both")) {
    type <- plot_type
  } else {
    type <- match.arg(type)
  }

  # set default values for arguments ------

  if (missing(style) && !is.null(attr(x, "theme"))) {
    theme_style <- unlist(strsplit(attr(x, "theme"), "::", fixed = TRUE))
    style <- get(theme_style[2], asNamespace(theme_style[1]))
  }

  if (is.null(colors)) {
    colors <- attr(x, "colors")
  }

  if (is.null(colors)) {
    colors <- c("#3aaf85", "#1b6ca8", "#cd201f")
  }

  colors <- unname(colors)

  if (is.null(alpha_level)) {
    alpha_level <- 0.2
  }

  if (is.null(dot_alpha_level)) {
    dot_alpha_level <- 0.8
  }

  if (is.null(check)) {
    check <- "all"
  }


  # build plot panels --------------------

  if ("PP_CHECK" %in% names(x) && !is.null(x$PP_CHECK) && any(c("pp_check", "all") %in% check)) {
    x$NORM <- NULL
    p$PP_CHECK <- plot.see_performance_pp_check(
      x$PP_CHECK,
      style = style,
      type = type,
      check_model = TRUE,
      adjust_legend = TRUE,
      colors = colors[1:2]
    )
  }

  if ("NCV" %in% names(x) && !is.null(x$NCV) && any(c("ncv", "linearity", "all") %in% check)) {
    p$NCV <- .plot_diag_linearity(
      x$NCV,
      size_point,
      size_line,
      alpha_level,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level,
      show_dots = show_dots
    )
  }

  if ("BINNED_RESID" %in% names(x) && !is.null(x$BINNED_RESID) && any(c("binned_residuals", "all") %in% check)) {
    x$HOMOGENEITY <- NULL
    p$BINNED_RESID <- plot.see_binned_residuals(
      x$BINNED_RESID,
      style = style,
      colors = colors[c(2, 3, 1)],
      adjust_legend = TRUE,
      check_model = TRUE,
      show_dots = show_dots
    )
  }

  if ("OVERDISPERSION" %in% names(x) && !is.null(x$OVERDISPERSION) && any(c("overdispersion", "all") %in% check)) {
    p$OVERDISPERSION <- .plot_diag_overdispersion(
      x$OVERDISPERSION,
      style = style,
      colors = colors[c(1, 2)],
      size_line = size_line,
      type = overdisp_type
    )
  }

  if ("HOMOGENEITY" %in% names(x) && !is.null(x$HOMOGENEITY) && any(c("homogeneity", "all") %in% check)) {
    p$HOMOGENEITY <- .plot_diag_homogeneity(
      x$HOMOGENEITY,
      size_point,
      size_line,
      alpha_level,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level,
      show_dots = show_dots
    )
  }

  if ("INFLUENTIAL" %in% names(x) && !is.null(x$INFLUENTIAL) && any(c("outliers", "influential", "all") %in% check)) {
    p$OUTLIERS <- .plot_diag_outliers_new(
      x$INFLUENTIAL,
      show_labels = show_labels,
      size_text = size_text,
      size_line = size_line,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level,
      show_dots = show_dots
    )
  }

  if ("VIF" %in% names(x) && !is.null(x$VIF) && any(c("vif", "all") %in% check)) {
    p$VIF <- .plot_diag_vif(
      x$VIF,
      size_point = 1.5 * size_point,
      size_line = size_line,
      theme_style = style,
      colors = colors,
      ci_data = attributes(x$VIF)$CI,
      is_check_model = TRUE
    )
  }

  if ("QQ" %in% names(x) && !is.null(x$QQ) && any(c("qq", "all") %in% check)) {
    p$QQ <- .plot_diag_qq(
      x$QQ,
      size_point,
      size_line,
      alpha_level = alpha_level,
      detrend = detrend,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level,
      show_dots = TRUE, # qq-plots w/o dots makes no sense
      model_info = model_info
    )
  }

  if ("NORM" %in% names(x) && !is.null(x$NORM) && any(c("normality", "all") %in% check)) {
    p$NORM <- .plot_diag_norm(
      x$NORM,
      size_line,
      alpha_level = alpha_level,
      theme_style = style,
      colors = colors
    )
  }

  if ("REQQ" %in% names(x) && !is.null(x$REQQ) && any(c("reqq", "all") %in% check)) {
    ps <- .plot_diag_reqq(
      x$REQQ,
      size_point,
      size_line,
      alpha_level = alpha_level,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level,
      show_dots = TRUE # qq-plots w/o dots makes no sense
    )

    for (i in seq_along(ps)) {
      p[[length(p) + 1]] <- ps[[i]]
    }
  }

  if (panel) {
    pw <- plots(p, n_columns = n_columns)
    .safe_print_plots(pw, ...)
    invisible(pw)
  } else {
    return(p)
  }
}


.plot_diag_linearity <- function(x,
                                 size_point,
                                 size_line,
                                 alpha_level = 0.2,
                                 theme_style = theme_lucid,
                                 colors = unname(social_colors(c("green", "blue", "red"))),
                                 dot_alpha_level = 0.8,
                                 show_dots = TRUE) {
  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, y = .data$y))

  if (isTRUE(show_dots)) {
    p <- p +
      geom_point2(
        colour = colors[2],
        size = size_point,
        alpha = dot_alpha_level
      )
  }

  p +
    ggplot2::geom_smooth(
      method = "loess",
      se = TRUE,
      formula = y ~ x,
      alpha = alpha_level,
      linewidth = size_line,
      colour = colors[1]
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Linearity",
      subtitle = "Reference line should be flat and horizontal"
    ) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    )
}
