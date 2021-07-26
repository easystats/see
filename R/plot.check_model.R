#' @importFrom rlang .data `%||%`
#' @export
plot.see_check_model <- function(x,
                                 style = theme_lucid,
                                 colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                 ...) {
  p <- list()

  panel <- attr(x, "panel")
  check <- attr(x, "check")
  size_point <- attr(x, "dot_size")
  size_line <- attr(x, "line_size")
  show_labels <- attr(x, "show_labels") %||% TRUE
  size_text <- attr(x, "text_size")
  alpha_level <- attr(x, "alpha")
  dot_alpha_level <- attr(x, "dot_alpha")
  detrend <- attr(x, "detrend")

  if (missing(style) && !is.null(attr(x, "theme"))) {
    theme_style <- unlist(strsplit(attr(x, "theme"), "::", fixed = TRUE))
    style <- get(theme_style[2], asNamespace(theme_style[1]))
  }

  if (missing(colors)) {
    colors <- attr(x, "colors")
  }

  if (is.null(colors)) {
    colors <- c("#3aaf85", "#1b6ca8", "#cd201f")
  }

  colors <- unname(colors)

  if (is.null(alpha_level)) {
    alpha_level <- .2
  }

  if (is.null(dot_alpha_level)) {
    dot_alpha_level <- .8
  }

  if (is.null(check)) check <- "all"

  if ("NCV" %in% names(x) && any(c("ncv", "linearity", "all") %in% check)) {
    p$NCV <- .plot_diag_linearity(
      x$NCV,
      size_point,
      size_line,
      alpha_level,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level
    )
  }

  if ("HOMOGENEITY" %in% names(x) && any(c("homogeneity", "all") %in% check)) {
    p$HOMOGENEITY <- .plot_diag_homogeneity(
      x$HOMOGENEITY,
      size_point,
      size_line,
      alpha_level,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level
    )
  }

  if ("VIF" %in% names(x) && any(c("vif", "all") %in% check)) {
    p$VIF <- .plot_diag_vif(
      x$VIF,
      theme_style = style,
      colors = colors
    )
  }

  if ("OUTLIERS" %in% names(x) && any(c("outliers", "all") %in% check)) {
    p$OUTLIERS <- .plot_diag_outliers_new(
      x$INFLUENTIAL,
      show_labels = show_labels,
      size_text = size_text,
      size_line = size_line,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level
    )
  }

  if ("QQ" %in% names(x) && any(c("qq", "all") %in% check)) {
    p$QQ <- .plot_diag_qq(
      x$QQ,
      size_point,
      size_line,
      alpha_level = alpha_level,
      detrend = detrend,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level
    )
  }

  if ("NORM" %in% names(x) && any(c("normality", "all") %in% check)) {
    p$NORM <- .plot_diag_norm(
      x$NORM,
      size_line,
      alpha_level = alpha_level,
      theme_style = style,
      colors = colors
    )
  }

  if ("REQQ" %in% names(x) && any(c("reqq", "all") %in% check)) {
    ps <- .plot_diag_reqq(
      x$REQQ,
      size_point,
      size_line,
      alpha_level = alpha_level,
      theme_style = style,
      colors = colors,
      dot_alpha_level = dot_alpha_level
    )

    for (i in 1:length(ps)) {
      p[[length(p) + 1]] <- ps[[i]]
    }
  }

  if (panel) {
    plots(p, n_columns = 2)
  } else {
    return(p)
  }
}



.plot_diag_vif <- function(x,
                           theme_style = theme_lucid,
                           colors = unname(social_colors(c("green", "blue", "red")))) {
  ylim <- max(x$y, na.rm = TRUE)
  if (ylim < 10) ylim <- 10

  # make sure legend is properly sorted
  x$group <- factor(x$group, levels = c("low", "moderate", "high"))
  levels(x$group) <- c("low (< 5)", "moderate (< 10)", "high (>= 10)")
  names(colors) <- c("low (< 5)", "moderate (< 10)", "high (>= 10)")

  p <- ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$group))

  if (ylim > 5) {
    p <- p + geom_rect(
      xmin = -Inf,
      xmax = Inf,
      ymin = 0,
      ymax = 5,
      fill = colors[1],
      color = NA,
      alpha = .025
    )

    p <- p + geom_rect(
      xmin = -Inf,
      xmax = Inf,
      ymin = 5,
      ymax = ifelse(ylim > 10, 10, ylim),
      fill = colors[2],
      color = NA,
      alpha = .025
    )
  }

  if (ylim > 10) {
    p <- p + geom_rect(
      xmin = -Inf,
      xmax = Inf,
      ymin = 10,
      ymax = ylim,
      fill = colors[3],
      color = NA,
      alpha = .025
    )
  }

  p <- p +
    geom_col(width = 0.7) +
    labs(
      title = "Collinearity",
      subtitle = "Higher bars (>5) indicate potential collinearity issues",
      x = NULL,
      y = "Variance Inflation Factor (VIF)",
      fill = NULL
    ) +
    # geom_text(aes(label = round(.data$y, 1)), nudge_y = 1) +
    scale_fill_manual(values = colors) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    ) +
    ylim(c(0, ylim)) +
    theme(
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, -5, -5, -5)
    )

  if ("facet" %in% colnames(x)) {
    p <- p + facet_wrap(~facet, nrow = 1, scales = "free")
  }

  p
}



.plot_diag_norm <- function(x,
                            size_line,
                            alpha_level = .2,
                            theme_style = theme_lucid,
                            colors = unname(social_colors(c("green", "blue", "red")))) {
  ggplot(x, aes(x = .data$x)) +
    geom_ribbon(
      mapping = aes(ymin = 0, ymax = .data$y),
      colour = NA,
      fill = colors[2],
      alpha = alpha_level
    ) +
    geom_line(
      mapping = aes(y = .data$curve),
      colour = colors[1],
      size = size_line
    ) +
    labs(
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
    scale_y_continuous(labels = NULL)
}




.plot_diag_qq <- function(x,
                          size_point,
                          size_line,
                          alpha_level = .2,
                          detrend = FALSE,
                          theme_style = theme_lucid,
                          colors = unname(social_colors(c("green", "blue", "red"))),
                          dot_alpha_level = .8) {
  if (requireNamespace("qqplotr", quietly = TRUE)) {
    qq_stuff <- list(
      qqplotr::stat_qq_band(alpha = alpha_level, detrend = detrend),
      qqplotr::stat_qq_line(
        size = size_line,
        colour = colors[1],
        detrend = detrend
      ),
      qqplotr::stat_qq_point(
        shape = 16,
        stroke = 0,
        size = size_point,
        colour = colors[2], # "#2c3e50",
        alpha = dot_alpha_level,
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
      geom_qq(
        shape = 16, stroke = 0,
        size = size_point,
        colour = colors[2] # "#2c3e50"
      ),
      geom_qq_line(
        size = size_line,
        colour = colors[1]
      )
    )
    y_lab <- "Sample Quantiles"
  }
  ggplot(x, aes(sample = .data$y)) +
    qq_stuff +
    labs(
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
                          alpha_level = .2,
                          detrend = FALSE,
                          theme_style = theme_lucid,
                          colors = unname(social_colors(c("green", "blue", "red"))),
                          dot_alpha_level = .8) {
  if (requireNamespace("qqplotr", quietly = TRUE)) {
    p_plot <- ggplot(x, aes(sample = .data$res)) +
      qqplotr::stat_pp_band(alpha = alpha_level, detrend = detrend) +
      qqplotr::stat_pp_line(
        size = size_line,
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

    p_plot <- ggplot(x, aes(x = .data$probs, y = .data$y)) +
      geom_abline(
        slope = 1,
        size = size_line,
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
    labs(
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
                                   alpha_level = .2,
                                   theme_style = theme_lucid,
                                   colors = unname(social_colors(c("green", "blue", "red"))),
                                   dot_alpha_level = .8) {
  ggplot(x, aes(x = .data$x, .data$y)) +
    geom_point2(
      colour = colors[2],
      size = size_point,
      alpha = dot_alpha_level
    ) +
    stat_smooth(
      method = "loess",
      se = TRUE,
      alpha = alpha_level,
      formula = y ~ x,
      size = size_line,
      colour = colors[1]
    ) +
    labs(
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
                                 alpha_level = .2,
                                 theme_style = theme_lucid,
                                 colors = unname(social_colors(c("green", "blue", "red"))),
                                 dot_alpha_level = .8) {
  ggplot(x, aes(x = .data$x, y = .data$y)) +
    geom_point2(
      colour = colors[2],
      size = size_point,
      alpha = dot_alpha_level
    ) +
    geom_smooth(
      method = "loess",
      se = TRUE,
      formula = y ~ x,
      alpha = alpha_level,
      size = size_line,
      colour = colors[1]
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
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
                            alpha_level = .2,
                            theme_style = theme_lucid,
                            colors = unname(social_colors(c("green", "blue", "red"))),
                            dot_alpha_level = .8) {
  lapply(names(x), function(i) {
    dat <- x[[i]]
    p <- ggplot(dat, aes(x = .data$x, y = .data$y)) +
      labs(
        x = "Theoretical Quantiles",
        y = "RE Quantiles",
        title = sprintf("Normality of Random Effects (%s)", i),
        subtitle = "Dots should be plotted along the line"
      ) +
      stat_smooth(
        method = "lm",
        alpha = alpha_level,
        size = size_line,
        formula = y ~ x,
        colour = colors[1]
      ) +
      geom_errorbar(
        aes(ymin = .data$conf.low, ymax = .data$conf.high),
        width = 0,
        colour = colors[2],
        alpha = dot_alpha_level
      ) +
      geom_point2(
        colour = colors[2],
        size = size_point,
        alpha = dot_alpha_level
      ) +
      theme_style(
        base_size = 10,
        plot.title.space = 3,
        axis.title.space = 5
      )

    if (nlevels(dat$facet) > 1 && isTRUE(panel)) {
      p <- p + facet_wrap(~facet, scales = "free")
    }

    p
  })
}
