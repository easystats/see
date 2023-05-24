#' @export
data_plot.performance_pp_check <- function(x, type = "line", ...) {
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
  dataplot$grp <- rep(seq_len(ncol(x)), each = nrow(x))

  attr(dataplot, "info") <- list(
    "xlab" = attr(x, "response_name"),
    "ylab" = ifelse(identical(type, "line"), "Density", "Counts"),
    "title" = "Posterior Predictive Check",
    "check_range" = attr(x, "check_range"),
    "bandwidth" = attr(x, "bandwidth"),
    "model_info" = attr(x, "model_info")
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
#' @param type Plot type for the posterior predictive checks plot. Can be `"line"`
#' (default) or `"dots"` (only for models with binary, integer or ordinal outcomes).
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_parameters_distribution
#'
#' @return A ggplot2-object.
#'
#' @examplesIf require("performance")
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' check_predictions(model)
#'
#' # dot-plot style for count-models
#' d <- iris
#' d$poisson_var <- rpois(150, 1)
#' model <- glm(
#'   poisson_var ~ Species + Petal.Length + Petal.Width,
#'   data = d,
#'   family = poisson()
#' )
#' out <- check_predictions(model)
#' plot(out, type = "dots")
#' @export
print.see_performance_pp_check <- function(x,
                                           size_line = 0.5,
                                           line_alpha = 0.15,
                                           size_bar = 0.7,
                                           style = theme_lucid,
                                           colors = unname(social_colors(c("green", "blue"))),
                                           type = c("line", "dots"),
                                           ...) {
  orig_x <- x
  check_range <- isTRUE(attributes(x)$check_range)
  type <- match.arg(type)

  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, type)
  }

  p1 <- .plot_pp_check(x, size_line, line_alpha, theme_style = style, colors = colors, type = type, ...)

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
                                          size_line = 0.5,
                                          line_alpha = 0.15,
                                          size_bar = 0.7,
                                          style = theme_lucid,
                                          colors = unname(social_colors(c("green", "blue"))),
                                          type = c("line", "dots"),
                                          ...) {
  orig_x <- x
  check_range <- isTRUE(attributes(x)$check_range)
  type <- match.arg(type)

  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, type)
  }

  p1 <- .plot_pp_check(x, size_line, line_alpha, theme_style = style, colors = colors, type = type, ...)

  if (isTRUE(check_range)) {
    p2 <- .plot_pp_check_range(orig_x, size_bar, colors = colors)
    plots(p1, p2)
  } else {
    p1
  }
}



.plot_pp_check <- function(x, size_line, line_alpha, theme_style, colors, type = "line", ...) {
  info <- attr(x, "info")

  # default bandwidth, for smooting
  bandwidth <- info$bandwidth
  if (is.null(bandwidth)) {
    bandwidth <- "nrd"
  }

  minfo <- info$model_info
  suggest_dots <- (minfo$is_bernoulli || minfo$is_count || minfo$is_ordinal || minfo$is_categorical)

  if (identical(type, "dots") && suggest_dots) {
    out <- .plot_check_predictions_dots(x, colors, info, size_line, line_alpha, ...)
  } else {
    if (suggest_dots) {
      insight::format_alert(
        "The model has an integer or a categorical response variable.",
        "It is recommended to switch to a dot-plot style, e.g. `plot(check_model(model), type = \"dots\"`."
      )
    }
    # denity plot - for models that have no binary or count/ordinal outcome
    out <- .plot_check_predictions_density(x, colors, info, size_line, line_alpha, bandwidth, ...)
  }


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


.plot_check_predictions_density <- function(x, colors, info, size_line, line_alpha, bandwidth, ...) {
  ggplot2::ggplot(x) +
    ggplot2::stat_density(
      mapping = ggplot2::aes(
        x = .data$values,
        group = .data$grp,
        color = .data$key,
        linewidth = .data$key,
        alpha = .data$key
      ),
      geom = "line",
      position = "identity",
      bw = bandwidth
    ) +
    ggplot2::scale_y_continuous() +
    ggplot2::scale_color_manual(values = c(
      "Observed data" = colors[1],
      "Model-predicted data" = colors[2]
    )) +
    ggplot2::scale_linewidth_manual(
      values = c(
        "Observed data" = 1.7 * size_line,
        "Model-predicted data" = size_line
      ),
      guide = "none"
    ) +
    ggplot2::scale_alpha_manual(
      values = c(
        "Observed data" = 1,
        "Model-predicted data" = line_alpha
      ),
      guide = "none"
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
}


.plot_check_predictions_dots <- function(x, colors, info, size_line, line_alpha, ...) {
  # make sure we have a factor, so "table()" generates frequencies for all levels
  # for each group - we need tables of same size to bind data frames
  x$values <- as.factor(x$values)
  x <- aggregate(x["values"], list(grp = x$grp), table)
  x <- cbind(data.frame(key = "Model-predicted data", stringsAsFactors = FALSE), x)
  x <- cbind(x[1:2], as.data.frame(x[[3]]))
  x$key[nrow(x)] <- "Observed data"
  x <- datawizard::data_to_long(x, select = -1:-2, names_to = "x", values_to = "count")
  if (insight::n_unique(x$x) > 8) {
    x$x <- datawizard::to_numeric(x$x)
  }

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = x[x$key == "Observed data", ],
      mapping = ggplot2::aes(x = .data$x, y = 0, group = .data$grp, yend = .data$count, xend = .data$x),
      colour = colors[1],
      size = 2.5 * size_line
    ) +
    ggplot2::geom_point(
      data = x[x$key == "Observed data", ],
      mapping = ggplot2::aes(x = .data$x, y = .data$count, group = .data$grp),
      colour = colors[1],
      size = 8 * size_line,
      stroke = 0,
      shape = 16
    ) +
    ggplot2::geom_point(
      data = x[x$key == "Model-predicted data", ],
      mapping = ggplot2::aes(x = .data$x, y = .data$count, group = .data$grp),
      color = colors[2],
      alpha = line_alpha,
      position = ggplot2::position_jitter(width = 0.1, height = 0.02),
      size = 5 * size_line,
      stroke = 0,
      shape = 16
    ) +
    ggplot2::scale_y_continuous() +
    ggplot2::scale_color_manual(values = c(
      "Observed data" = colors[1],
      "Model-predicted data" = colors[2]
    )) +
    ggplot2::labs(
      x = info$xlab,
      y = info$ylab,
      color = "",
      size = "",
      alpha = "",
      title = "Posterior Predictive Check",
      subtitle = "Model-predicted points should be close to observed data points"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = TRUE),
      size = ggplot2::guide_legend(reverse = TRUE)
    )
}


.plot_pp_check_range <- function(x,
                                 size_bar = 0.7,
                                 colors = unname(social_colors(c("green", "blue")))) {
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
      linewidth = 1
    ) +
    ggplot2::labs(
      x = NULL, y = NULL,
      subtitle = "Model-predicted extrema should contain observed data extrema"
    )
}
