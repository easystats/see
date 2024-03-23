#' @export
data_plot.performance_pp_check <- function(x, type = "density", ...) {
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
    xlab = attr(x, "response_name"),
    ylab = ifelse(identical(type, "density"), "Density", "Counts"),
    title = "Posterior Predictive Check",
    check_range = attr(x, "check_range"),
    bandwidth = attr(x, "bandwidth"),
    model_info = attr(x, "model_info")
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
#' @param type Plot type for the posterior predictive checks plot. Can be `"density"`
#' (default), `"discrete_dots"`, `"discrete_interval"` or `"discrete_both"` (the
#' `discrete_*` options are appropriate for models with discrete - binary, integer
#' or ordinal etc. - outcomes).
#' @param x_limits Numeric vector of length 2 specifying the limits of the x-axis.
#' If not `NULL`, will zoom in the x-axis to the specified limits.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_parameters_distribution
#'
#' @return A ggplot2-object.
#'
#' @seealso See also the vignette about [`check_model()`](https://easystats.github.io/performance/articles/check_model.html).
#'
#' @examples
#' library(performance)
#'
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
#' plot(out, type = "discrete_dots")
#' @export
print.see_performance_pp_check <- function(x,
                                           size_line = 0.5,
                                           size_point = 2,
                                           line_alpha = 0.15,
                                           size_bar = 0.7,
                                           style = theme_lucid,
                                           colors = unname(social_colors(c("green", "blue"))),
                                           type = c("density", "discrete_dots", "discrete_interval", "discrete_both"),
                                           x_limits = NULL,
                                           ...) {
  orig_x <- x
  check_range <- isTRUE(attributes(x)$check_range)
  plot_type <- attributes(x)$type

  if (missing(type) && !is.null(plot_type) && plot_type %in% c("density", "discrete_dots", "discrete_interval", "discrete_both")) {
    type <- plot_type
  } else {
    type <- match.arg(type)
  }

  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, type)
  }

  p1 <- .plot_pp_check(
    x,
    size_line,
    size_point,
    line_alpha,
    theme_style = style,
    colors = colors,
    type = type,
    x_limits = x_limits,
    ...
  )

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
                                          size_point = 2,
                                          line_alpha = 0.15,
                                          size_bar = 0.7,
                                          style = theme_lucid,
                                          colors = unname(social_colors(c("green", "blue"))),
                                          type = c("density", "discrete_dots", "discrete_interval", "discrete_both"),
                                          x_limits = NULL,
                                          ...) {
  orig_x <- x
  check_range <- isTRUE(attributes(x)$check_range)
  plot_type <- attributes(x)$type

  if (missing(type) && !is.null(plot_type) && plot_type %in% c("density", "discrete_dots", "discrete_interval", "discrete_both")) { # nolint
    type <- plot_type
  } else {
    type <- match.arg(type)
  }

  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, type)
  }

  p1 <- .plot_pp_check(
    x,
    size_line,
    size_point,
    line_alpha,
    theme_style = style,
    colors = colors,
    type = type,
    x_limits = x_limits,
    ...
  )

  if (isTRUE(check_range)) {
    p2 <- .plot_pp_check_range(orig_x, size_bar, colors = colors)
    plots(p1, p2)
  } else {
    p1
  }
}



.plot_pp_check <- function(x,
                           size_line,
                           size_point,
                           line_alpha,
                           theme_style,
                           colors,
                           type = "density",
                           x_limits = NULL,
                           ...) {
  info <- attr(x, "info")

  # default bandwidth, for smooting
  bandwidth <- info$bandwidth
  if (is.null(bandwidth)) {
    bandwidth <- "nrd"
  }

  minfo <- info$model_info
  suggest_dots <- (minfo$is_bernoulli || minfo$is_count || minfo$is_ordinal || minfo$is_categorical)

  if (!is.null(type) && type %in% c("discrete_dots", "discrete_interval", "discrete_both") && suggest_dots) {
    out <- .plot_check_predictions_dots(x, colors, info, size_line, size_point, line_alpha, type, ...)
  } else {
    if (suggest_dots) {
      insight::format_alert(
        "The model has an integer or a discrete response variable.",
        "It is recommended to switch to a dot-plot style, e.g. `plot(check_model(model), type = \"discrete_dots\"`."
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

  if (!is.null(x_limits)) {
    out <- out + ggplot2::coord_cartesian(xlim = x_limits)
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


.plot_check_predictions_dots <- function(x,
                                         colors,
                                         info,
                                         size_line,
                                         size_point,
                                         line_alpha,
                                         type = "discrete_dots",
                                         ...) {
  # make sure we have a factor, so "table()" generates frequencies for all levels
  # for each group - we need tables of same size to bind data frames
  x$values <- as.factor(x$values)
  x <- stats::aggregate(x["values"], list(grp = x$grp), table)
  x <- cbind(data.frame(key = "Model-predicted data", stringsAsFactors = FALSE), x)
  x <- cbind(x[1:2], as.data.frame(x[[3]]))
  x$key[nrow(x)] <- "Observed data"
  x <- datawizard::data_to_long(x, select = -1:-2, names_to = "x", values_to = "count")
  if (insight::n_unique(x$x) > 8) {
    x$x <- datawizard::to_numeric(x$x)
  }

  p1 <- p2 <- NULL

  if (!is.null(type) && type %in% c("discrete_interval", "discrete_both")) {
    centrality_dispersion <- function(i) {
      c(
        count = stats::median(i, na.rm = TRUE),
        unlist(bayestestR::ci(i)[c("CI_low", "CI_high")])
      )
    }
    x_errorbars <- stats::aggregate(x["count"], list(x$x), centrality_dispersion)
    x_errorbars <- cbind(x_errorbars[1], as.data.frame(x_errorbars[[2]]))
    colnames(x_errorbars) <- c("x", "count", "CI_low", "CI_high")
    x_errorbars <- cbind(
      data.frame(key = "Model-predicted data", stringsAsFactors = FALSE),
      x_errorbars
    )

    x_tmp <- x[x$key == "Observed data", ]
    x_tmp$CI_low <- NA
    x_tmp$CI_high <- NA
    x_tmp$grp <- NULL

    x_errorbars <- rbind(x_errorbars, x_tmp)
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_pointrange(
        data = x_errorbars[x_errorbars$key == "Model-predicted data", ],
        mapping = ggplot2::aes(
          x = .data$x,
          y = .data$count,
          ymin = .data$CI_low,
          ymax = .data$CI_high,
          color = .data$key
        ),
        position = ggplot2::position_nudge(x = 0.2),
        size = size_point,
        linewidth = size_line,
        stroke = 0,
        shape = 16
      ) +
      ggplot2::geom_point(
        data = x_errorbars[x_errorbars$key == "Observed data", ],
        mapping = ggplot2::aes(
          x = .data$x,
          y = .data$count,
          color = .data$key
        ),
        size = size_point,
        stroke = 0,
        shape = 16
      )
  }

  if (!is.null(type) && type %in% c("discrete_dots", "discrete_both")) {
    if (is.null(p1)) {
      p2 <- ggplot2::ggplot()
    } else {
      p2 <- p1
    }
    p2 <- p2 + ggplot2::geom_point(
      data = x[x$key == "Model-predicted data", ],
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$count,
        group = .data$grp,
        color = .data$key
      ),
      alpha = line_alpha,
      position = ggplot2::position_jitter(width = 0.1, height = 0.02),
      size = size_point * 0.8,
      stroke = 0,
      shape = 16
    ) +
      # for legend
      ggplot2::geom_point(
        data = x[x$key == "Observed data", ],
        mapping = ggplot2::aes(
          x = .data$x,
          y = .data$count,
          group = .data$grp,
          color = .data$key
        ),
        size = size_point * 0.8
      ) +
      ggplot2::geom_point(
        data = x[x$key == "Observed data", ],
        mapping = ggplot2::aes(
          x = .data$x,
          y = .data$count
        ),
        size = size_point,
        shape = 21,
        colour = "white",
        fill = colors[1]
      )
  }

  if (is.null(p2)) {
    p <- p1
  } else {
    p <- p2
  }

  if (type == "discrete_interval") {
    subtitle <- "Model-predicted intervals should include observed data points"
  } else {
    subtitle <- "Model-predicted points should be close to observed data points"
  }

  p <- p +
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
      subtitle = subtitle
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = TRUE),
      size = ggplot2::guide_legend(reverse = TRUE)
    )

  return(p)
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
      x = vapply(x[which(names(x) != "y")], min, numeric(1)),
      group = "Minimum",
      color = "Model-predicted data",
      stringsAsFactors = FALSE
    ),
    data.frame(
      x = vapply(x[which(names(x) != "y")], max, numeric(1)),
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
