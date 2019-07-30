#' @param checks Character vector, indicating which checks for \code{performance::check_model()}
#'   should be performed and plotted. May be one or more of
#'   \code{"all", "vif", "qq", "normality", "ncv", "homogeneity", "outliers", "reqq"}.
#'   \code{"reqq"} is a QQ-plot for random effects and only available for mixed models.
#'   \code{"ncv"} checks for non-constant variance, i.e. for heteroscedasticity.
#'   By default, all possible checks are performed and plotted.
#' @importFrom rlang .data
#' @importFrom graphics plot
#' @rdname data_plot
#' @export
print.see_check_model <- function(x, checks = "all", ...) {
  p <- list()

  panel <- attr(x, "panel")
  dot_size <- attr(x, "dot_size")
  line_size <- attr(x, "line_size")
  text_size <- attr(x, "text_size")

  checks <- tolower(checks)

  if ("VIF" %in% names(x) && c("vif", "all") %in% checks) p$VIF <- .plot_diag_vif(x$VIF)
  if ("QQ" %in% names(x) && c("qq", "all") %in% checks) p$QQ <- .plot_diag_qq(x$QQ, dot_size, line_size)
  if ("NORM" %in% names(x) && c("normality", "all") %in% checks) p$NORM <- .plot_diag_norm(x$NORM, line_size)
  if ("NCV" %in% names(x) && c("ncv", "all") %in% checks) p$NCV <- .plot_diag_ncv(x$NCV, dot_size, line_size)
  if ("HOMOGENEITY" %in% names(x) && c("homogeneity", "all") %in% checks) p$HOMOGENEITY <- .plot_diag_homogeneity(x$HOMOGENEITY, dot_size, line_size)
  if ("OUTLIERS" %in% names(x) && c("outliers", "all") %in% checks) {
    p$OUTLIERS <- .plot_diag_outliers(x$OUTLIERS, text_size)
    p$OUTLIERS <- p$OUTLIERS +
      theme_lucid(
        base_size = 10,
        plot.title.space = 3,
        axis.title.space = 5
      )
  }

  if ("REQQ" %in% names(x) && c("reqq", "all") %in% checks) {
    ps <- .plot_diag_reqq(x$REQQ, dot_size, line_size)
    for (i in 1:length(ps)) {
      p[[length(p) + 1]] <- ps[[i]]
    }
  }

  if (panel) {
    do.call(plots, p)
  } else {
    lapply(p, graphics::plot)
  }
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



.plot_diag_norm <- function(x, line_size) {
  ggplot(x, aes(x = .data$x)) +
    geom_ribbon(
      mapping = aes(ymin = 0, ymax = .data$y),
      colour = unname(flat_colors("blue grey")),
      fill = unname(flat_colors("light blue")),
      alpha = 0.2
    ) +
    geom_line(
      mapping = aes(y = .data$curve),
      colour = unname(flat_colors("teal")),
      size = line_size
    ) +
    labs(
      x = "Residuals",
      y = "Density",
      title = "Non-Normality of Residuals",
      subtitle = "Distribution should look like a normal curve"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}



.plot_diag_qq <- function(x, dot_size, line_size) {
  ggplot(x, aes(x = .data$x, y = .data$y)) +
    stat_smooth(method = "lm", size = line_size, colour = unname(flat_colors("teal"))) +
    geom_point2(colour = "#2c3e50", size = dot_size) +
    labs(
      title = "Non-normality of Residuals and Outliers",
      subtitle = "Dots should be plotted along the line",
      y = "(Studentized) Residuals",
      x = "Theoretical Quantiles"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}




.plot_diag_pp <- function(x, dot_size, line_size) {
  ggplot(x, aes(x = .data$x, y = .data$y)) +
    stat_smooth(method = "lm", size = line_size, colour = unname(flat_colors("teal"))) +
    geom_point2(colour = "#2c3e50", size = dot_size) +
    labs(
      title = "Non-normality of Residuals and Outliers (PP plot)",
      subtitle = "Dots should be plotted along the line",
      y = "Residuals",
      x = "Theoretical Quantiles"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}




.plot_diag_homogeneity <- function(x, dot_size, line_size) {
  ggplot(x, aes(x = .data$x, .data$y)) +
    geom_point2(colour = "#2c3e50", size = dot_size) +
    stat_smooth(method = "loess", se = FALSE, size = line_size, colour = unname(flat_colors("dark red"))) +
    labs(
      title = "Homogeneity of Variance (Scale-Location)",
      subtitle = "Dots should spread equally around horizontal line",
      y = "Std. Residuals (sqrt)",
      x = "Fitted values"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}



.plot_diag_ncv <- function(x, dot_size, line_size) {
  ggplot(x, aes(x = .data$x, y = .data$y)) +
    geom_point2(colour = "#2c3e50", size = dot_size) +
    geom_smooth(method = "loess", se = FALSE, size = line_size, colour = unname(flat_colors("dark red"))) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Homoscedasticity (Linear Relationship)",
      subtitle = "Dots should spread equally around horizontal line"
    ) +
    theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
}



.plot_diag_reqq <- function(x, dot_size, line_size) {
  lapply(names(x), function(i) {
    dat <- x[[i]]
    p <- ggplot(dat, aes(x = .data$x, y = .data$y)) +
      labs(
        x = "Theoretical Quantiles",
        y = "RE Quantiles",
        title = sprintf("Normality of Random Effects (%s)", i),
        subtitle = "Dots should be plotted along the line"
      ) +
      geom_errorbar(
        aes(ymin = .data$conf.low, ymax = .data$conf.high),
        width = 0,
        colour = "#2c3e50"
      ) +
      geom_point2(colour = "#2c3e50", size = dot_size) +
      stat_smooth(
        method = "lm",
        alpha = .2,
        size = line_size,
        colour = unname(flat_colors("teal"))
      ) +
      theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)

    if (nlevels(dat$facet) > 1) {
      p <- p + facet_wrap(~facet, scales = "free")
    }

    p
  })
}