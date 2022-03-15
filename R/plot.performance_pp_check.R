#' @export
data_plot.performance_pp_check <- function(x, ...) {
  columns <- colnames(x)
  dataplot <- stats::reshape(
    x,
    times = columns,
    timevar = "key",
    v.names = "values",
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dataplot[["values"]])) {
    dataplot[["values"]] <- as.character(dataplot[["values"]])
  }

  dataplot <- dataplot[, 1:(ncol(dataplot) - 1), drop = FALSE]
  dataplot$key[dataplot$key != "y"] <- "Model-predicted data"
  dataplot$key[dataplot$key == "y"] <- "Observed data"
  dataplot$grp <- rep(1:ncol(x), each = nrow(x))

  attr(dataplot, "info") <- list(
    "xlab" = attr(x, "response_name"),
    "ylab" = "Density",
    "title" = "Posterior Predictive Check",
    "check_range" = attr(x, "check_range")
  )

  class(dataplot) <- unique(c("data_plot", "see_performance_pp_check", class(dataplot)))
  dataplot
}


# Plot --------------------------------------------------------------------

#' Plot method for posterior predictive checks
#'
#' The `plot()` method for the `performance::check_predictions()` function.
#'
#' @param line_alpha Numeric value specifying alpha of lines indicating `yrep`.
#' @param style A ggplot2-theme.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_parameters_distribution
#'
#' @return A ggplot2-object.
#'
#' @examples
#' if (require("performance")) {
#'   model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#'   check_posterior_predictions(model)
#' }
#' @export
print.see_performance_pp_check <- function(x,
                                           size_line = .5,
                                           line_alpha = .15,
                                           size_bar = 0.7,
                                           style = theme_lucid,
                                           colors = unname(social_colors(c("green", "blue"))),
                                           ...) {
  orig_x <- x
  check_range <- isTRUE(attributes(x)$check_range)

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  p1 <- .plot_pp_check(x, size_line, line_alpha, theme_style = style, colors = colors, ...)

  if (isTRUE(check_range)) {
    p2 <- .plot_pp_check_range(orig_x, size_bar, colors = colors)
    graphics::plot(plots(p1, p2, n_columns = 1))
  } else {
    suppressWarnings(graphics::plot(p1))
  }

  invisible(orig_x)
}


#' @rdname print.see_performance_pp_check
#' @export
plot.see_performance_pp_check <- function(x,
                                          size_line = .5,
                                          line_alpha = .15,
                                          size_bar = 0.7,
                                          style = theme_lucid,
                                          colors = unname(social_colors(c("green", "blue"))),
                                          ...) {
  orig_x <- x
  check_range <- isTRUE(attributes(x)$check_range)

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  p1 <- .plot_pp_check(x, size_line, line_alpha, theme_style = style, colors = colors, ...)

  if (isTRUE(check_range)) {
    p2 <- .plot_pp_check_range(orig_x, size_bar, colors = colors)
    plots(p1, p2)
  } else {
    p1
  }
}



.plot_pp_check <- function(x, size_line, line_alpha, theme_style, colors, ...) {
  info <- attr(x, "info")

  out <- ggplot2::ggplot(x) +
    ggplot2::stat_density(
      mapping = ggplot2::aes(
        x = .data$values,
        group = .data$grp,
        color = .data$key,
        size = .data$key,
        alpha = .data$key
      ),
      geom = "line",
      position = "identity",
    ) +
    ggplot2::scale_y_continuous() +
    ggplot2::scale_color_manual(values = c(
      "Observed data" = colors[1],
      "Model-predicted data" = colors[2]
    )) +
    ggplot2::scale_size_manual(
      values = c(
        "Observed data" = 2 * size_line,
        "Model-predicted data" = size_line
      ),
    ) +
    ggplot2::scale_alpha_manual(
      values = c(
        "Observed data" = 1,
        "Model-predicted data" = line_alpha
      ), guide = "none"
    ) +
    ggplot2::labs(
      x = info$xlab,
      y = info$ylab,
      color = "",
      size = "",
      alpha = "",
      title = "Posterior Predictive Check",
      subtitle = "Model-predicted lines should resemble observed data line"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = TRUE),
      size = ggplot2::guide_legend(reverse = TRUE)
    )


  dots <- list(...)
  if (isTRUE(dots[["check_model"]])) {
    out <- out + theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    )
  }

  if (isTRUE(dots[["adjust_legend"]]) || isTRUE(info$check_range)) {
    out <- out + ggplot2::theme(
      legend.position = "bottom",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(-5, -5, -5, -5)
    )
  }

  out
}


.plot_pp_check_range <- function(x, size_bar = .7, colors) {
  original <-
    data.frame(
      x = c(min(x$y), max(x$y)),
      group = factor(c("Minimum", "Maximum"), levels = c("Minimum", "Maximum")),
      color = "Observed data",
      stringsAsFactors = FALSE
    )

  replicated <- rbind(
    data.frame(
      x = sapply(x[which(names(x) != "y")], min),
      group = "Minimum",
      color = "Model-predicted data",
      stringsAsFactors = FALSE
    ),
    data.frame(
      x = sapply(x[which(names(x) != "y")], max),
      group = "Maximum",
      color = "Model-predicted data",
      stringsAsFactors = FALSE
    )
  )
  replicated$group <- factor(replicated$group, levels = c("Minimum", "Maximum"))

  p <- ggplot2::ggplot(replicated, ggplot2::aes(x = .data$x, group = .data$group)) +
    ggplot2::facet_wrap(~group, scales = "free_x")

  if (insight::n_unique(replicated$x) <= 12) {
    p <- p + ggplot2::geom_bar(width = size_bar, fill = colors[2], color = NA)
  } else if (.is_integer(replicated$x)) {
    p <- p +
      ggplot2::geom_bar(width = size_bar, fill = colors[2], color = NA) +
      ggplot2::scale_x_continuous(n.breaks = round(insight::n_unique(replicated$x) / 4))
  } else {
    p <- p + ggplot2::geom_histogram(binwidth = size_bar, fill = colors[2], color = NA)
  }

  p +
    ggplot2::geom_vline(
      data = original,
      mapping = ggplot2::aes(xintercept = .data$x),
      color = colors[1],
      size = 1
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      subtitle = "Model-predicted extrema should contain observed data extrema"
    )
}
