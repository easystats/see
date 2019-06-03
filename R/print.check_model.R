#' @importFrom rlang .data
#' @importFrom graphics plot
#' @export
print.see_check_model <- function(x, ...) {
  p <- list()
  panel <- attr(x, "panel")

  if ("VIF" %in% names(x)) p$VIF <- .plot_diag_vif(x$VIF)
  if ("QQ" %in% names(x)) p$QQ <- .plot_diag_qq(x$QQ)
  if ("NORM" %in% names(x)) p$NORM <- .plot_diag_norm(x$NORM)
  if ("NCV" %in% names(x)) p$NCV <- .plot_diag_ncv(x$NCV)
  if ("HOMOGENEITY" %in% names(x)) p$HOMOGENEITY <- .plot_diag_homogeneity(x$HOMOGENEITY)

  if ("REQQ" %in% names(x)) {
    ps <- .plot_diag_reqq(x$REQQ)
    for (i in 1:length(ps)) {
      p[[length(p) + 1]] <- ps[[i]]
    }
  }

  if (panel) {
    do.call(plots, p)
  } else {
    suppressWarnings(grpahics::plot(p))
  }
}



.plot_diag_vif <- function(x) {
  ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$group)) +
    geom_col(width = 0.7) +
    labs(title = "Check for Multicollinearity", x = NULL, y = NULL, fill = "Correlation") +
    scale_fill_manual(values = unname(flat_colors("green", "orange", "red"))) +
    theme_light(base_size = 10)
}



.plot_diag_norm <- function(x) {
  ggplot(x, aes(x = .data$x)) +
    geom_ribbon(
      mapping = aes(ymin = 0, ymax = .data$y),
      colour = unname(flat_colors("blue grey")),
      fill = unname(flat_colors("light blue")),
      alpha = 0.2
    ) +
    geom_line(
      mapping = aes(y = .data$curve),
      colour = unname(flat_colors("blue")),
      size = 0.8
    ) +
    labs(
      x = "Residuals",
      y = "Density",
      title = "Non-Normality of Residuals",
      subtitle = "Distribution should look like a normal curve"
    ) +
    theme_light(base_size = 10)
}



.plot_diag_qq <- function(x) {
  ggplot(x, aes(x = .data$x, y = .data$y)) +
    geom_point2(colour = unname(flat_colors("dark red"))) +
    stat_smooth(method = "lm", se = FALSE, size = .9, colour = unname(flat_colors("blue"))) +
    labs(
      title = "Non-normality of Residuals and Outliers",
      subtitle = "Dots should be plotted along the line",
      y = "(Studentized) Residuals",
      x = "Theoretical quantiles (predicted values)"
    ) +
    theme_light(base_size = 10)
}




.plot_diag_homogeneity <- function(x) {
  ggplot(x, aes(x = .data$x, .data$y)) +
    geom_point2(colour = unname(flat_colors("dark red"))) +
    stat_smooth(method = "loess", se = FALSE, size = .9, colour = unname(flat_colors("blue"))) +
    labs(
      title = "Homogeneity of Variance (Scale-Location)",
      subtitle = "Dots should spread equally around a horizontal line",
      y = "Std. Residuals (sqrt)",
      x = "Fitted values"
    ) +
    theme_light(base_size = 10)
}



.plot_diag_ncv <- function(x) {
  ggplot(x, aes(x = .data$x, y = .data$y)) +
    geom_point2(colour = unname(flat_colors("dark red"))) +
    geom_smooth(method = "loess", se = FALSE, size = .9, colour = unname(flat_colors("blue"))) +
    labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Homoscedasticity (Linear Relationship)",
      subtitle = "Dots should show no distinct patterns around a horizontal line"
    ) +
    theme_light(base_size = 10)
}



.plot_diag_reqq <- function(x) {
  lapply(names(x), function(i) {
    dat <- x[[i]]
    p <- ggplot(dat, aes(x = .data$x, y = .data$y)) +
      labs(
        x = "Theoretical quantiles",
        y = "RE quantiles",
        title = sprintf("Normality of Random Effects (%s)", i),
        subtitle = "Dots should be plotted along the line"
      ) +
      geom_errorbar(
        aes(ymin = .data$conf.low, ymax = .data$conf.high),
        width = 0,
        colour = unname(flat_colors("dark red"))
      ) +
      geom_point2(colour = unname(flat_colors("dark red"))) +
      stat_smooth(
        method = "lm",
        alpha = .2,
        colour = unname(flat_colors("blue"))
      ) +
      theme_light(base_size = 10)

    if (nlevels(dat$facet) > 1) {
      p <- p + facet_wrap(~facet, scales = "free")
    }

    p
  })
}