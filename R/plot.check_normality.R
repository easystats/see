#' Plot method for check model for (non-)normality of residuals
#'
#' The `plot()` method for the `performance::check_normality()`
#' function.
#'
#' @param type Character vector, indicating the type of plot.
#'   Options are `"qq"` (default) for quantile-quantile (Q-Q) plots,
#'   `"pp"` for probability-probability (P-P) plots, or
#'   `"density"` for density overlay plots.
#' @param linewidth Numeric value specifying size of line geoms.
#' @param alpha_dot Numeric value specifying alpha level of the point geoms.
#' @param alpha Numeric value specifying alpha level of the confidence bands.
#' @param colors Character vector of length two, indicating the colors (in
#'   hex-format) for points and line.
#' @param detrend Logical that decides if Q-Q and P-P plots should be de-trended
#'   (also known as _worm plots_).
#' @param method The method used for estimating the qq/pp bands. Default to
#'   `"ell"` (equal local levels / simultaneous testing - recommended). Can also
#'   be one of `"pointwise"` or `"boot"` for pointwise confidence bands, or
#'   `"ks"` or `"ts"` for simultaneous testing. See `qqplotr::stat_qq_band()`
#'   for details.
#' @param base_size,size_axis_title,size_title Numeric value specifying size of
#' axis and plot titles.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#'
#' @return A ggplot2-object.
#'
#' @seealso See also the vignette about [`check_model()`](https://easystats.github.io/performance/articles/check_model.html).
#'
#' @examples
#' library(performance)
#'
#' m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- check_normality(m)
#' plot(result)
#'
#' @examplesIf require("qqplotr")
#' plot(result, type = "qq", detrend = TRUE)
#'
#' @export
plot.see_check_normality <- function(
  x,
  type = "qq",
  data = NULL,
  linewidth = 0.8,
  size_point = 2,
  size_title = 12,
  size_axis_title = base_size,
  base_size = 10,
  alpha = 0.2,
  alpha_dot = 0.8,
  colors = c("#3aaf85", "#1b6ca8"),
  detrend = TRUE,
  method = "ell",
  ...
) {
  type <- insight::validate_argument(type, c("qq", "pp", "density"))

  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }

  # for GLM, only halfnormal Q-Q plots
  if (inherits(model, "glm")) {
    type <- "qq"
  }

  # check type
  if (!is.null(attributes(x)$effects) && attributes(x)$effects == "random") {
    .plot_diag_reqq(
      attributes(x)$re_qq,
      size_point = size_point,
      linewidth = linewidth,
      alpha_level = alpha,
      size_axis_title = size_axis_title,
      size_title = size_title,
      base_size = base_size
    )
  } else if (type == "qq") {
    # return early for simres
    if (inherits(model, "performance_simres")) {
      return(plot.see_performance_simres(
        model,
        linewidth = linewidth,
        size_point = size_point,
        alpha = alpha,
        alpha_dot = alpha_dot,
        colors = colors,
        detrend = detrend,
        base_size = base_size,
        transform = stats::qnorm,
        ...
      ))
    }

    model_info <- attributes(x)$model_info
    dat <- .residuals_qq(model)
    .plot_diag_qq(
      dat,
      size_point = size_point,
      linewidth = linewidth,
      size_axis_title = size_axis_title,
      size_title = size_title,
      base_size = base_size,
      alpha_level = alpha,
      detrend = detrend,
      alpha_dot = alpha_dot,
      model_info = model_info,
      method = method,
      model_class = class(model)[1]
    )
  } else if (type == "density") {
    dat <- .residuals_density(model)
    .plot_diag_norm(
      dat,
      linewidth = linewidth,
      alpha_level = alpha,
      base_size = base_size,
      size_axis_title = size_axis_title,
      size_title = size_title
    )
  } else if (type == "pp") {
    dat <- .residuals_pp(model)
    .plot_diag_pp(
      dat,
      size_point = size_point,
      linewidth = linewidth,
      base_size = base_size,
      size_axis_title = size_axis_title,
      size_title = size_title,
      alpha_level = alpha,
      detrend = detrend,
      alpha_dot = alpha_dot,
      method = method
    )
  }
}


# extract residuals

.residuals_qq <- function(model) {
  if (inherits(model, c("lme", "lmerMod", "merMod", "afex_aov", "BFBayesFactor"))) {
    res_ <- suppressMessages(sort(stats::residuals(model), na.last = NA))
    dat <- stats::na.omit(data.frame(y = res_))
  } else if (inherits(model, "glmmTMB")) {
    res_ <- abs(stats::residuals(model, type = "deviance"))
    dat <- stats::na.omit(data.frame(y = res_))
  } else if (inherits(model, "glm")) {
    res_ <- abs(stats::rstandard(model, type = "deviance"))
    fitted_ <- stats::qnorm(
      (stats::ppoints(length(res_)) + 1) / 2
    )[order(order(res_))]
    dat <- stats::na.omit(data.frame(x = fitted_, y = res_))
  } else if (.is_efa(model)) {
    res_ <- suppressMessages(sort(insight::get_residuals(model), na.last = NA))
    dat <- stats::na.omit(data.frame(y = res_))
  } else if (is.numeric(model)) {
    res_ <- sort(model[!is.infinite(model)])
    dat <- stats::na.omit(data.frame(y = res_))
  } else {
    res_ <- sort(stats::rstudent(model), na.last = NA)
    dat <- stats::na.omit(data.frame(y = res_))
  }
  dat
}


.residuals_density <- function(model) {
  if (inherits(model, "performance_simres")) {
    r <- stats::residuals(model, quantile_function = stats::qnorm)
    r <- r[!is.infinite(r)]
  } else if (is.numeric(model)) {
    r <- model[!is.infinite(model) & !is.na(model)]
  } else if (.is_efa(model)) {
    r <- insight::get_residuals(model)
  } else {
    r <- suppressMessages(stats::residuals(model))
  }
  dat <- as.data.frame(bayestestR::estimate_density(r))
  dat$curve <- stats::dnorm(
    seq(min(dat$x), max(dat$x), length.out = nrow(dat)),
    mean(r),
    stats::sd(r)
  )
  dat
}


.residuals_pp <- function(model) {
  if (.is_efa(model)) {
    x <- suppressMessages(sort(insight::get_residuals(model), na.last = NA))
  } else {
    x <- suppressMessages(sort(stats::residuals(model), na.last = NA))
  }
  data.frame(res = x)
}


.is_efa <- function(model = NULL, mclass = NULL) {
  efa_classes <- c("fa", "principal", "omega", "parameters_efa", "parameters_omega")
  if (is.null(mclass)) {
    inherits(model, efa_classes)
  } else {
    any(tolower(mclass) %in% efa_classes)
  }
}


# normality plot: density -------------------------

.plot_diag_norm <- function(
  x,
  linewidth,
  size_axis_title = 10,
  size_title = 12,
  alpha_level = 0.2,
  theme_style = theme_lucid,
  base_size = 10,
  colors = unname(social_colors(c("green", "blue", "red")))
) {
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
      linewidth = linewidth,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      x = "Residuals",
      y = "Density",
      title = "Normality of Residuals",
      subtitle = "Distribution should be close to the normal curve"
    ) +
    theme_style(
      base_size = base_size,
      plot.title.space = 3,
      axis.title.space = 5,
      plot.title.size = size_title,
      axis.title.size = size_axis_title
    ) +
    ggplot2::scale_y_continuous(labels = NULL)
}


# normality plot: QQ -------------------------

.plot_diag_qq <- function(
  x,
  size_point,
  linewidth,
  size_axis_title = 10,
  size_title = 12,
  alpha_level = 0.2,
  detrend = FALSE,
  method = "ell",
  theme_style = theme_lucid,
  base_size = 10,
  colors = unname(social_colors(c("green", "blue", "red"))),
  alpha_dot = 0.8,
  show_dots = TRUE,
  model_info = NULL,
  model_class = NULL
) {
  qhalfnorm <- function(p) stats::qnorm((p + 1) / 2)

  # set default y-range for FA / PCA
  if (!is.null(model_class) && .is_efa(model = NULL, mclass = model_class)) {
    if (any(abs(x$y) > 0.075)) {
      y_range <- c(-max(abs(x$y)), max(abs(x$y)))
    } else {
      y_range <- c(-0.075, 0.075)
    }
  } else {
    y_range <- NULL
  }

  # qq-halfnorm for GLM
  if (isTRUE(model_info$is_binomial) || isTRUE(model_info$is_count)) {
    gg_init <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, y = .data$y))
    qq_stuff <- list(
      ggplot2::geom_point(
        shape = 16,
        stroke = 0,
        size = size_point,
        colour = colors[2]
      ),
      ggplot2::geom_qq_line(
        ggplot2::aes(sample = .data$y),
        linewidth = linewidth,
        colour = colors[1],
        distribution = qhalfnorm,
        na.rm = TRUE
      )
    )
    y_lab <- "|Std. Deviance Residuals|"
  } else if (requireNamespace("qqplotr", quietly = TRUE)) {
    gg_init <- ggplot2::ggplot(x, ggplot2::aes(sample = .data$y))
    qq_stuff <- list(
      qqplotr::stat_qq_band(
        alpha = alpha_level,
        detrend = detrend,
        bandType = method
      ),
      qqplotr::stat_qq_point(
        shape = 16,
        stroke = 0,
        size = size_point,
        colour = colors[2],
        alpha = alpha_dot,
        detrend = detrend
      ),
      qqplotr::stat_qq_line(
        linewidth = linewidth,
        colour = colors[1],
        detrend = detrend
      )
    )
    if (detrend) {
      y_lab <- "Sample Quantile Deviations"
    } else {
      y_lab <- "Sample Quantiles"
    }
  } else {
    insight::format_alert("For confidence bands, please install `qqplotr`.")

    # to scale the detrended qq plot
    N <- length(x$y)
    SD <- stats::sd(x$y) * sqrt((N - 1) / N)
    y_range <- round(range(x$y), 1)

    gg_init <- ggplot2::ggplot(x, ggplot2::aes(sample = .data$y))

    qq_stuff <- list(
      if (detrend) {
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth = linewidth,
          colour = colors[1],
          na.rm = TRUE
        )
      } else {
        ggplot2::geom_qq_line(
          linewidth = linewidth,
          colour = colors[1],
          na.rm = TRUE
        )
      },
      ggplot2::geom_qq(
        mapping = if (detrend) {
          ggplot2::aes(
            x = ggplot2::after_stat(.data$theoretical * SD),
            y = ggplot2::after_stat(.data$sample - .data$theoretical * SD)
          )
        },
        shape = 16,
        na.rm = TRUE,
        stroke = 0,
        size = size_point,
        colour = colors[2]
      )
    )

    if (detrend) {
      y_lab <- "Sample Quantile Deviations"
    } else {
      y_lab <- "Sample Quantiles"
      y_range <- NULL
    }
  }

  if (!isTRUE(show_dots)) {
    qq_stuff[2] <- NULL
  }

  p <- gg_init +
    qq_stuff +
    ggplot2::labs(
      title = "Normality of Residuals",
      subtitle = "Dots should fall along the line",
      y = y_lab,
      x = "Standard Normal Distribution Quantiles"
    ) +
    theme_style(
      base_size = base_size,
      plot.title.space = 3,
      axis.title.space = 5,
      plot.title.size = size_title,
      axis.title.size = size_axis_title
    )

  if (!is.null(y_range)) {
    p <- p + ggplot2::ylim(y_range)
  }

  p
}


# normality plot: PP -------------------------

.plot_diag_pp <- function(
  x,
  size_point,
  linewidth,
  size_axis_title = base_size,
  size_title = 12,
  alpha_level = 0.2,
  detrend = FALSE,
  method = "ell",
  theme_style = theme_lucid,
  base_size = 10,
  colors = unname(social_colors(c("green", "blue", "red"))),
  alpha_dot = 0.8
) {
  if (requireNamespace("qqplotr", quietly = TRUE)) {
    p_plot <- ggplot2::ggplot(x, ggplot2::aes(sample = .data$res)) +
      qqplotr::stat_pp_band(
        alpha = alpha_level,
        detrend = detrend,
        bandType = method
      ) +
      qqplotr::stat_pp_line(
        linewidth = linewidth,
        colour = colors[1],
        detrend = detrend
      ) +
      qqplotr::stat_pp_point(
        shape = 16,
        stroke = 0,
        size = size_point,
        colour = colors[2],
        alpha = alpha_dot,
        detrend = detrend
      )
  } else if (requireNamespace("MASS", quietly = TRUE)) {
    insight::format_alert("For confidence bands, please install `qqplotr`.")

    x$probs <- stats::ppoints(x$res)
    dparms <- MASS::fitdistr(x$res, densfun = "normal")
    x$y <- do.call(stats::pnorm, c(list(q = x$res), dparms$estimate))

    p_plot <- ggplot2::ggplot(x, ggplot2::aes(x = .data$probs, y = .data$y)) +
      ggplot2::geom_abline(
        slope = if (detrend) 0 else 1,
        linewidth = linewidth,
        colour = colors[1]
      ) +
      geom_point2(
        mapping = if (detrend) ggplot2::aes(y = .data$y - .data$probs),
        colour = colors[2],
        size = size_point,
        alpha = alpha_dot
      )
  } else {
    insight::format_error(
      "Package 'qqplotr' OR 'MASS' required for P-P plots. Please install one of them."
    )
  }

  y_lab <- "Sample Cummulative Probability"
  if (detrend) {
    y_lab <- paste0(y_lab, " Deviations")
  }

  p_plot +
    ggplot2::labs(
      title = "Normality of Residuals",
      subtitle = "Dots should fall along the line",
      y = y_lab,
      x = "Standard Normal Cumulative Probability"
    ) +
    theme_style(
      base_size = base_size,
      plot.title.space = 3,
      axis.title.space = 5,
      plot.title.size = size_title,
      axis.title.size = size_axis_title
    )
}


# normality plot: Random Effects QQ -------------------------

.plot_diag_reqq <- function(
  x,
  size_point,
  linewidth,
  size_axis_title = base_size,
  size_title = 12,
  panel = TRUE,
  alpha_level = 0.2,
  theme_style = theme_lucid,
  base_size = 10,
  colors = unname(social_colors(c("green", "blue", "red"))),
  alpha_dot = 0.8,
  show_dots = TRUE
) {
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
        linewidth = linewidth,
        formula = y ~ x,
        colour = colors[1]
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$conf.low, ymax = .data$conf.high),
        width = 0,
        colour = colors[2],
        alpha = alpha_dot
      ) +
      theme_style(
        base_size = base_size,
        plot.title.space = 3,
        axis.title.space = 5,
        plot.title.size = size_title,
        axis.title.size = size_axis_title
      )

    if (isTRUE(show_dots)) {
      p <- p +
        geom_point2(
          colour = colors[2],
          size = size_point,
          alpha = alpha_dot
        )
    }

    if (nlevels(dat$facet) > 1 && isTRUE(panel)) {
      p <- p + ggplot2::facet_wrap(~facet, scales = "free")
    }

    p
  })
}
