#' @importFrom ggplot2 .data
#' @export
plot.see_check_model <- function(x,
                                 style = theme_lucid,
                                 colors = NULL,
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
      show_dots = TRUE # qq-plots w/o dots makes no sense
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
    pw <- plots(p, n_columns = 2)
    .safe_print_plots(pw)
    invisible(pw)
  } else {
    return(p)
  }
}



.plot_diag_vif <- function(x,
                           size_point,
                           size_line,
                           theme_style = theme_lucid,
                           colors = unname(social_colors(c("green", "blue", "red"))),
                           ci_data = NULL,
                           is_check_model = FALSE) {
  ylim <- ceiling(max(x$y, na.rm = TRUE))
  xlim <- nrow(x)
  if (ylim < 10) ylim <- 10

  if (!is.null(ci_data)) {
    x <- cbind(x, ci_data)
  } else {
    x$VIF_CI_low <- NA_real_
    x$VIF_CI_high <- NA_real_
  }

  # make sure legend is properly sorted
  x$group <- factor(x$group, levels = c("low", "moderate", "high"))
  levels(x$group) <- c("Low (< 5)", "Moderate (< 10)", "High (\u2265 10)")
  names(colors) <- c("Low (< 5)", "Moderate (< 10)", "High (\u2265 10)")

  p <- ggplot2::ggplot(x) +
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      color = .data$group,
      ymin = .data$VIF_CI_low,
      ymax = .data$VIF_CI_high
    ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 1,
      ymax = 5,
      fill = colors[1],
      color = NA,
      alpha = 0.15
    ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 5,
      ymax = 10,
      fill = colors[2],
      color = NA,
      alpha = 0.15
    ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 10,
      ymax = Inf,
      fill = colors[3],
      color = NA,
      alpha = 0.15
    ) +
    {
      if (!is.null(ci_data)) {
        list(
          ggplot2::geom_linerange(
            linewidth = size_line,
            na.rm = TRUE
          ),
          ggplot2::geom_segment(
            data = x[x$VIF_CI_high > ylim * 1.15, ],
            mapping = aes(
              x = .data$x,
              xend = .data$x,
              y = .data$y,
              yend = .data$VIF_CI_high
            ),
            lineend = "round",
            linejoin = "round",
            arrow = ggplot2::arrow(
              ends = "last", type = "closed",
              angle = 20, length = ggplot2::unit(0.03, "native")
            ),
            show.legend = FALSE
          )
        )
      }
    } +
    geom_point2(
      size = size_point,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      title = "Collinearity",
      subtitle = "High collinearity (VIF) may inflate parameter uncertainty",
      x = NULL,
      y = paste("Variance Inflation", "Factor (VIF, log-scaled)", sep = ifelse(is_check_model, "\n", " "))
    ) +
    ggplot2::scale_color_manual(
      values = colors,
      aesthetics = c("color", "fill"),
      guide = ggplot2::guide_legend(title = NULL)
    ) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    ) +
    ggplot2::scale_y_continuous(
      limits = c(1, ylim * 1.15),
      oob = scales::oob_squish,
      trans = "log10",
      expand = c(0, 0),
      breaks = scales::log_breaks(n = 7, base = 10)
    ) +
    ggplot2::scale_x_discrete() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(-5, -5, -5, -5)
    )

  if ("facet" %in% colnames(x)) {
    p <- p + ggplot2::facet_wrap(~facet, nrow = 1, scales = "free")
  }

  p
}



.plot_diag_norm <- function(x,
                            size_line,
                            alpha_level = 0.2,
                            theme_style = theme_lucid,
                            colors = unname(social_colors(c("green", "blue", "red")))) {
  ggplot2::ggplot(x, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymin = 0, ymax = .data$y),
      colour = NA,
      fill = colors[2],
      alpha = alpha_level,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(y = .data$curve),
      colour = colors[1],
      linewidth = size_line,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      x = "Residuals",
      y = "Density",
      title = "Normality of Residuals",
      subtitle = "Distribution should be close to the normal curve"
    ) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    ) +
    ggplot2::scale_y_continuous(labels = NULL)
}




.plot_diag_qq <- function(x,
                          size_point,
                          size_line,
                          alpha_level = 0.2,
                          detrend = FALSE,
                          theme_style = theme_lucid,
                          colors = unname(social_colors(c("green", "blue", "red"))),
                          dot_alpha_level = 0.8,
                          show_dots = TRUE) {
  if (requireNamespace("qqplotr", quietly = TRUE)) {
    qq_stuff <- list(
      qqplotr::stat_qq_band(
        alpha = alpha_level,
        detrend = detrend
      ),
      qqplotr::stat_qq_point(
        shape = 16,
        stroke = 0,
        size = size_point,
        colour = colors[2], # "#2c3e50",
        alpha = dot_alpha_level,
        detrend = detrend
      ),
      qqplotr::stat_qq_line(
        linewidth = size_line,
        colour = colors[1],
        detrend = detrend
      )
    )
    y_lab <- "Sample Quantiles"
  } else {
    message(
      "For confidence bands",
      if (isTRUE(detrend)) " and detrending",
      ", please install `qqplotr`."
    )

    qq_stuff <- list(
      ggplot2::geom_qq_line(
        linewidth = size_line,
        colour = colors[1],
        na.rm = TRUE
      ),
      ggplot2::geom_qq(
        shape = 16,
        na.rm = TRUE,
        stroke = 0,
        size = size_point,
        colour = colors[2] # "#2c3e50"
      )
    )
    y_lab <- "Sample Quantiles"
  }

  if (!isTRUE(show_dots)) {
    qq_stuff[2] <- NULL
  }

  ggplot2::ggplot(x, ggplot2::aes(sample = .data$y)) +
    qq_stuff +
    ggplot2::labs(
      title = "Normality of Residuals",
      subtitle = "Dots should fall along the line",
      y = y_lab,
      x = "Standard Normal Distribution Quantiles"
    ) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    )
}




.plot_diag_pp <- function(x,
                          size_point,
                          size_line,
                          alpha_level = 0.2,
                          detrend = FALSE,
                          theme_style = theme_lucid,
                          colors = unname(social_colors(c("green", "blue", "red"))),
                          dot_alpha_level = 0.8) {
  if (requireNamespace("qqplotr", quietly = TRUE)) {
    p_plot <- ggplot2::ggplot(x, ggplot2::aes(sample = .data$res)) +
      qqplotr::stat_pp_band(alpha = alpha_level, detrend = detrend) +
      qqplotr::stat_pp_line(
        linewidth = size_line,
        colour = colors[1],
        detrend = detrend
      ) +
      qqplotr::stat_pp_point(
        shape = 16, stroke = 0,
        size = size_point,
        colour = colors[2], # "#2c3e50",
        alpha = dot_alpha_level,
        detrend = detrend
      )
  } else if (requireNamespace("MASS", quietly = TRUE)) {
    message(
      "For confidence bands",
      if (isTRUE(detrend)) " and detrending",
      ", please install `qqplotr`."
    )

    x$probs <- stats::ppoints(x$res)
    dparms <- MASS::fitdistr(x$res, densfun = "normal")
    x$y <- do.call(stats::pnorm, c(list(q = x$res), dparms$estimate))

    p_plot <- ggplot2::ggplot(x, ggplot2::aes(x = .data$probs, y = .data$y)) +
      ggplot2::geom_abline(
        slope = 1,
        linewidth = size_line,
        colour = colors[1]
      ) +
      geom_point2(
        colour = colors[2],
        size = size_point,
        alpha = dot_alpha_level
      ) # "#2c3e50"
  } else {
    stop("Package 'qqplotr' OR 'MASS' required for PP-plots. Please install one of them.", call. = FALSE)
  }

  p_plot +
    ggplot2::labs(
      title = "Normality of Residuals (PP plot)",
      subtitle = "Dots should fall along the line",
      y = "Cummulative Probability",
      x = "Probability Points"
    ) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    )
}




.plot_diag_homogeneity <- function(x,
                                   size_point,
                                   size_line,
                                   alpha_level = 0.2,
                                   theme_style = theme_lucid,
                                   colors = unname(social_colors(c("green", "blue", "red"))),
                                   dot_alpha_level = 0.8,
                                   show_dots = TRUE) {
  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, .data$y))

  if (isTRUE(show_dots)) {
    p <- p +
      geom_point2(
        colour = colors[2],
        size = size_point,
        alpha = dot_alpha_level
      )
  }

  p +
    ggplot2::stat_smooth(
      method = "loess",
      se = TRUE,
      alpha = alpha_level,
      formula = y ~ x,
      linewidth = size_line,
      colour = colors[1]
    ) +
    ggplot2::labs(
      title = "Homogeneity of Variance",
      subtitle = "Reference line should be flat and horizontal",
      y = expression(sqrt("|Std. residuals|")),
      x = "Fitted values"
    ) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    )
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



.plot_diag_reqq <- function(x,
                            size_point,
                            size_line,
                            panel = TRUE,
                            alpha_level = 0.2,
                            theme_style = theme_lucid,
                            colors = unname(social_colors(c("green", "blue", "red"))),
                            dot_alpha_level = 0.8,
                            show_dots = TRUE) {
  lapply(names(x), function(i) {
    dat <- x[[i]]
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::labs(
        x = "Theoretical Quantiles",
        y = "RE Quantiles",
        title = sprintf("Normality of Random Effects (%s)", i),
        subtitle = "Dots should be plotted along the line"
      ) +
      ggplot2::stat_smooth(
        method = "lm",
        alpha = alpha_level,
        linewidth = size_line,
        formula = y ~ x,
        colour = colors[1]
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high),
        width = 0,
        colour = colors[2],
        alpha = dot_alpha_level
      ) +
      theme_style(
        base_size = 10,
        plot.title.space = 3,
        axis.title.space = 5
      )

    if (isTRUE(show_dots)) {
      p <- p +
        geom_point2(
          colour = colors[2],
          size = size_point,
          alpha = dot_alpha_level
        )
    }


    if (nlevels(dat$facet) > 1 && isTRUE(panel)) {
      p <- p + ggplot2::facet_wrap(~facet, scales = "free")
    }

    p
  })
}



.plot_diag_overdispersion <- function(x,
                                      theme_style = theme_lucid,
                                      colors = c("#3aaf85", "#1b6ca8"),
                                      size_line = 0.8,
                                      type = 1,
                                      ...) {
  if (is.null(type) || type == 1) {
    p <- ggplot2::ggplot(x) +
      ggplot2::aes(x = .data$Predicted) +
      ggplot2::geom_smooth(ggplot2::aes(y = .data$V), linewidth = size_line, color = colors[2], se = FALSE) +
      ggplot2::geom_smooth(ggplot2::aes(y = .data$Res2), linewidth = size_line, color = colors[1]) +
      ggplot2::labs(
        title = "Overdispersion and zero-inflation",
        subtitle = "Observed residual variance (green) should follow predicted residual variance (blue)",
        x = "Predicted mean",
        y = "Residual variance"
      ) +
      theme_style(
        base_size = 10,
        plot.title.space = 3,
        axis.title.space = 5
      )
  } else {
    p <- ggplot2::ggplot(x) +
      ggplot2::aes(x = .data$Predicted) +
      ggplot2::geom_point(ggplot2::aes(y = .data$StdRes)) +
      ggplot2::geom_hline(
        yintercept = c(-2, 2, -4, 4),
        linetype = c("solid", "solid", "dashed", "dashed"),
        color = c(rep(colors[1], 2), rep(colors[2], 2))
      ) +
      ggplot2::labs(
        title = "Overdispersion and zero-inflation",
        subtitle = "Most points should be within solid lines, few points outside dashed lines",
        x = "Predicted mean",
        y = "Standardized resiuduals"
      ) +
      theme_style(
        base_size = 10,
        plot.title.space = 3,
        axis.title.space = 5
      )
  }

  p
}
