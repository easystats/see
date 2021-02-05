#' @importFrom rlang .data
#' @importFrom graphics plot
#' @export
print.see_check_model <- function(x, ...) {
  orig_x <- x
  p <- list()

  panel <- attr(x, "panel")
  check <- attr(x, "check")
  size_point <- attr(x, "dot_size")
  size_line <- attr(x, "line_size")
  size_text <- attr(x, "text_size")
  alpha_level <- attr(x, "alpha")

  if (is.null(alpha_level)) {
    alpha_level <- .2
  }

  if (is.null(check)) check <- "all"

  if ("VIF" %in% names(x) && any(c("vif", "all") %in% check)) p$VIF <- .plot_diag_vif(x$VIF)
  if ("QQ" %in% names(x) && any(c("qq", "all") %in% check)) p$QQ <- .plot_diag_qq(x$QQ, size_point, size_line, alpha_level = alpha_level)
  if ("NORM" %in% names(x) && any(c("normality", "all") %in% check)) p$NORM <- .plot_diag_norm(x$NORM, size_line, alpha_level = alpha_level)
  if ("NCV" %in% names(x) && any(c("ncv", "all") %in% check)) p$NCV <- .plot_diag_ncv(x$NCV, size_point, size_line)
  if ("HOMOGENEITY" %in% names(x) && any(c("homogeneity", "all") %in% check)) p$HOMOGENEITY <- .plot_diag_homogeneity(x$HOMOGENEITY, size_point, size_line)
  if ("OUTLIERS" %in% names(x) && any(c("outliers", "all") %in% check)) {
    p$OUTLIERS <- .plot_diag_outliers(x$OUTLIERS, size_text)
    p$OUTLIERS <- p$OUTLIERS +
      theme_lucid(
        base_size = 10,
        plot.title.space = 3,
        axis.title.space = 5
      )
  }

  if ("REQQ" %in% names(x) && any(c("reqq", "all") %in% check)) {
    ps <- .plot_diag_reqq(x$REQQ, size_point, size_line, alpha_level = alpha_level)
    for (i in 1:length(ps)) {
      p[[length(p) + 1]] <- ps[[i]]
    }
  }

  if (panel) {
    do.call(plots, p)
  } else {
    lapply(p, graphics::plot)
  }
  invisible(orig_x)
}



.plot_diag_vif <- function(x) {
  ylim <- max(x$y, na.rm = TRUE)
  if (ylim < 10) ylim <- 10

  # make sure legend is properly sorted
  x$group <- factor(x$group, levels = c("low", "moderate", "high"))
  colors <- unname(flat_colors("green", "orange", "red"))
  names(colors) <- c("low", "moderate", "high")

  p <- ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$group)) +
    geom_col(width = 0.7) +
    labs(title = "Check for Multicollinearity", x = NULL, y = NULL, fill = "Correlation") +
    scale_fill_manual(values = colors) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5) +
    ylim(c(0, ylim))

  if ("facet" %in% colnames(x)) {
    p <- p + facet_wrap(~facet, nrow = 1, scales = "free")
  }

  p
}



.plot_diag_norm <- function(x, size_line, alpha_level = .2) {
  ggplot(x, aes(x = .data$x)) +
    geom_ribbon(
      mapping = aes(ymin = 0, ymax = .data$y),
      colour = unname(flat_colors("blue grey")),
      fill = unname(flat_colors("light blue")),
      alpha = alpha_level
    ) +
    geom_line(
      mapping = aes(y = .data$curve),
      colour = unname(flat_colors("teal")),
      size = size_line
    ) +
    labs(
      x = "Residuals",
      y = "Density",
      title = "Non-Normality of Residuals",
      subtitle = "Distribution should look like a normal curve"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}



.plot_diag_qq <- function(x, size_point, size_line, alpha_level = .2) {
  if (requireNamespace("qqplotr")) {
    qq_stuff <- list(
      qqplotr::stat_qq_band(alpha = alpha_level),
      qqplotr::stat_qq_line(
        size = size_line,
        colour = unname(flat_colors("teal"))
      ),
      qqplotr::stat_qq_point(
        shape = 16, stroke = 0,
        size = size_point,
        colour = "#2c3e50"
      )
    )
  } else {
    message("For confidence bands, please install `qqplotr`.")
    qq_stuff <- list(
      geom_qq(
        shape = 16, stroke = 0,
        size = size_point,
        colour = "#2c3e50"
      ),
      geom_qq_line(
        size = size_line,
        colour = unname(flat_colors("teal"))
      )
    )
  }
  ggplot(x, aes(sample = .data$y)) +
    qq_stuff +
    labs(
      title = "Non-normality of Residuals",
      subtitle = "Dots should be plotted along the line",
      y = "Sample Quantiles",
      x = "Theoretical Quantiles"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}




.plot_diag_pp <- function(x, size_point, size_line, alpha_level = .2) {
  if (requireNamespace("qqplotr", quietly = TRUE)) {
    p_plot <- ggplot(x, aes(sample = .data$res)) +
      qqplotr::stat_pp_band(alpha = alpha_level) +
      qqplotr::stat_pp_line(
        size = size_line,
        colour = unname(flat_colors("teal"))
      ) +
      qqplotr::stat_pp_point(
        shape = 16, stroke = 0,
        size = size_point,
        colour = "#2c3e50"
      )
  } else if (requireNamespace("MASS", quietly = TRUE)) {
    message("For confidence bands, please install `qqplotr`.")

    x$probs <- stats::ppoints(x$res)
    dparms <- MASS::fitdistr(x$res, densfun = "normal")
    x$y <- do.call(stats::pnorm, c(list(q = x$res), dparms$estimate))

    p_plot <- ggplot(x, aes(x = .data$probs, y = .data$y)) +
      geom_abline(
        slope = 1,
        size = size_line,
        colour = unname(flat_colors("teal"))
      ) +
      geom_point2(colour = "#2c3e50", size = size_point)
  } else {
    stop("Package 'qqplotr' OR 'MASS' required for PP-plots. Please install one of them.", call. = FALSE)
  }

  p_plot +
    labs(
      title = "Non-normality of Residuals (PP plot)",
      subtitle = "Dots should be plotted along the line",
      y = "Cummulative Probability",
      x = "Probability Points"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}




.plot_diag_homogeneity <- function(x, size_point, size_line) {
  ggplot(x, aes(x = .data$x, .data$y)) +
    geom_point2(colour = "#2c3e50", size = size_point) +
    stat_smooth(method = "loess", se = FALSE, size = size_line, colour = unname(flat_colors("dark red"))) +
    labs(
      title = "Homogeneity of Variance (Scale-Location)",
      subtitle = "Dots should spread equally around horizontal line",
      y = "Std. Residuals (sqrt)",
      x = "Fitted values"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}



.plot_diag_ncv <- function(x, size_point, size_line) {
  ggplot(x, aes(x = .data$x, y = .data$y)) +
    geom_point2(colour = "#2c3e50", size = size_point) +
    geom_smooth(method = "loess", se = FALSE, size = size_line, colour = unname(flat_colors("dark red"))) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Homoscedasticity (Linear Relationship)",
      subtitle = "Dots should spread equally around horizontal line"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}



.plot_diag_reqq <- function(x, size_point, size_line, panel = TRUE, alpha_level = .2) {
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
        colour = unname(flat_colors("teal"))
      ) +
      geom_errorbar(
        aes(ymin = .data$conf.low, ymax = .data$conf.high),
        width = 0,
        colour = "#2c3e50"
      ) +
      geom_point2(colour = "#2c3e50", size = size_point) +
      theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)

    if (nlevels(dat$facet) > 1 && isTRUE(panel)) {
      p <- p + facet_wrap(~facet, scales = "free")
    }

    p
  })
}
