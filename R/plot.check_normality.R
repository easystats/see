#' Plot method for check model for (non-)normality of residuals
#'
#' The `plot()` method for the `performance::check_normality()`
#' function.
#'
#' @param type Character vector, indicating the type of plot.
#' @param size_line Numeric value specifying size of line geoms.
#' @param dot_alpha Numeric value specifying alpha level of the point geoms.
#' @param alpha Numeric value specifying alpha level of the confidence bands.
#' @param colors Character vector of length two, indicating the colors (in
#'   hex-format) for points and line.
#' @param detrend Logical that decides if the plot should be detrended.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#'
#' @return A ggplot2-object.
#'
#' @seealso See also the vignette about [`check_model()`](https://easystats.github.io/performance/articles/check_model.html).
#'
#' @examplesIf require("performance")
#' m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- check_normality(m)
#' plot(result)
#' @export
plot.see_check_normality <- function(x,
                                     type = c("density", "qq", "pp"),
                                     data = NULL,
                                     size_line = 0.8,
                                     size_point = 2,
                                     alpha = 0.2,
                                     dot_alpha = 0.8,
                                     colors = c("#3aaf85", "#1b6ca8"),
                                     detrend = FALSE,
                                     ...) {
  type <- match.arg(type)

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
      size_line = size_line,
      alpha_level = alpha
    )
  } else {
    if (type == "qq") {
      model_info <- attributes(x)$model_info
      if (inherits(model, c("lme", "lmerMod", "merMod", "glmmTMB", "afex_aov", "BFBayesFactor"))) {
        res_ <- suppressMessages(sort(stats::residuals(model), na.last = NA))
        dat <- stats::na.omit(data.frame(y = res_))
      } else if (inherits(model, "glm")) {
        res_ <- abs(stats::rstandard(model, type = "deviance"))
        fitted_ <- stats::qnorm((stats::ppoints(length(res_)) + 1) / 2)[order(order(res_))]
        dat <- stats::na.omit(data.frame(x = fitted_, y = res_))
      } else {
        res_ <- sort(stats::rstudent(model), na.last = NA)
        dat <- stats::na.omit(data.frame(y = res_))
      }

      .plot_diag_qq(
        dat,
        size_point = size_point,
        size_line = size_line,
        alpha_level = alpha,
        detrend = detrend,
        dot_alpha_level = dot_alpha,
        model_info = model_info
      )
    } else if (type == "density") {
      r <- suppressMessages(stats::residuals(model))
      dat <- as.data.frame(bayestestR::estimate_density(r))
      dat$curve <- stats::dnorm(
        seq(min(dat$x), max(dat$x), length.out = nrow(dat)),
        mean(r),
        stats::sd(r)
      )
      .plot_diag_norm(dat, size_line = size_line)
    } else if (type == "pp") {
      x <- suppressMessages(sort(stats::residuals(model), na.last = NA))
      dat <- data.frame(res = x)
      .plot_diag_pp(
        dat,
        size_point = size_point,
        size_line = size_line,
        alpha_level = alpha,
        detrend = detrend,
        dot_alpha_level = dot_alpha
      )
    }
  }
}


# normality plot: density -------------------------

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


# normality plot: QQ -------------------------

.plot_diag_qq <- function(x,
                          size_point,
                          size_line,
                          alpha_level = 0.2,
                          detrend = FALSE,
                          theme_style = theme_lucid,
                          colors = unname(social_colors(c("green", "blue", "red"))),
                          dot_alpha_level = 0.8,
                          show_dots = TRUE,
                          model_info = NULL) {
  qhalfnorm <- function(p) stats::qnorm((p + 1) / 2)
  # qq-halfnorm for GLM
  if (isTRUE(model_info$is_binomial) || isTRUE(model_info$is_count)) {
    gg_init <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, y = .data$y))
    qq_stuff <- list(
      ggplot2::geom_point(
        shape = 16,
        stroke = 0,
        size = size_point,
        colour = colors[2] # "#2c3e50"
      ),
      ggplot2::geom_qq_line(
        ggplot2::aes(sample = .data$y),
        linewidth = size_line,
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
    insight::format_alert(
      paste0(
        "For confidence bands",
        if (isTRUE(detrend)) " and detrending",
        ", please install `qqplotr`."
      )
    )

    gg_init <- ggplot2::ggplot(x, ggplot2::aes(sample = .data$y))
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

  gg_init +
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


# normality plot: PP -------------------------

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


# normality plot: Random Effects QQ -------------------------

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
