#' @export
data_plot.parameters_brms_meta <- function(x, data = NULL, normalize_height = TRUE, ...) {
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  colnames(data) <- attributes(x)$cleaned_parameters
  dataplot <- bayestestR::estimate_density(data)

  if ("Parameter" %in% names(dataplot)) {
    colnames(dataplot)[match("Parameter", colnames(dataplot))] <- "Study"
  }

  dataplot$Study <- factor(dataplot$Study, levels = rev(unique(dataplot$Study)))

  # normalize height
  if (isTRUE(normalize_height)) {
    dataplot$y <- datawizard::rescale(dataplot$y, to = c(0, 0.9))
  }

  # summary
  summary <- x[, 1:6]
  summary$Parameter <- attributes(x)$cleaned_parameters
  colnames(summary)[2] <- "Estimate"
  summary$Estimate_CI <- sprintf(
    "%.2f %s",
    summary$Estimate,
    insight::format_ci(
      summary$CI_low,
      summary$CI_high,
      ci = NULL,
      digits = 2,
      zap_small = TRUE
    )
  )

  summary$Parameter <- factor(summary$Parameter, levels = rev(unique(summary$Parameter)))
  colnames(summary)[match("Parameter", colnames(summary))] <- "Study"

  summary$x <- NA_real_
  summary$y <- NA_real_
  summary$Color <- "Study"
  summary$Color[summary$Study == "Overall"] <- "Overall"

  if ("ROPE_low" %in% names(x) && "ROPE_high" %in% names(x)) {
    attr(summary, "rope") <- c(x$ROPE_low[1], x$ROPE_high[1])
  }

  dataplot <- dataplot[dataplot$Study != "tau", ]
  summary <- summary[summary$Study != "tau", ]

  dataplot$Study <- droplevels(dataplot$Study)
  summary$Study <- droplevels(summary$Study)

  dataplot$Group <- "Study"
  dataplot$Group[dataplot$Study == "Overall"] <- "Overall"
  dataplot$Color <- "Study"
  dataplot$Color[dataplot$Study == "Overall"] <- "Overall"

  attr(dataplot, "summary") <- summary
  attr(dataplot, "info") <- list(
    "xlab" = "Standardized Mean Difference",
    "ylab" = NULL,
    "legend_fill" = NULL,
    "legend_color" = NULL,
    "title" = "Bayesian Meta-Analysis"
  )

  class(dataplot) <- unique(c("data_plot", "see_parameters_brms_meta", class(dataplot)))
  dataplot
}




# Plot --------------------------------------------------------------------

#' Plot method for Model Parameters from Bayesian Meta-Analysis
#'
#' The `plot()` method for the `parameters::model_parameters()`
#' function when used with brms-meta-analysis models.
#'
#' @param normalize_height Logical. If `TRUE`, height of mcmc-areas is
#'   "normalized", to avoid overlap. In certain cases when the range of a
#'   posterior distribution is narrow for some parameters, this may result in
#'   very flat mcmc-areas. In such cases, set `normalize_height = FALSE`.
#' @inheritParams data_plot
#' @inheritParams plot.see_rope
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_check_outliers
#' @inheritParams plot.see_estimate_density
#'
#' @return A ggplot2-object.
#'
#' @details
#' \subsection{Colors of density areas and errorbars}{
#'   To change the colors of the density areas, use `scale_fill_manual()`
#'   with named color-values, e.g. `scale_fill_manual(values = c("Study" =
#'   "blue", "Overall" = "green"))`.
#'   To change the color of the error bars, use `scale_color_manual(values
#'   = c("Errorbar" = "red"))`.
#' }
#' \subsection{Show or hide estimates and CI}{
#'   Use `show_labels = FALSE` to hide the textual
#'   output of estimates and credible intervals.
#' }
#'
#' @examplesIf require("brms") && require("metafor")
#' \dontrun{
#' library(parameters)
#' library(brms)
#' library(metafor)
#' data(dat.bcg)
#'
#' dat <- escalc(
#'   measure = "RR",
#'   ai = tpos,
#'   bi = tneg,
#'   ci = cpos,
#'   di = cneg,
#'   data = dat.bcg
#' )
#' dat$author <- make.unique(dat$author)
#'
#' # model
#' set.seed(123)
#' priors <- c(
#'   prior(normal(0, 1), class = Intercept),
#'   prior(cauchy(0, 0.5), class = sd)
#' )
#' model <- brm(yi | se(vi) ~ 1 + (1 | author), data = dat)
#'
#' # result
#' mp <- model_parameters(model)
#' plot(mp)
#' }
#' @export
plot.see_parameters_brms_meta <- function(x,
                                          size_point = 2,
                                          size_line = 0.8,
                                          size_text = 3.5,
                                          posteriors_alpha = 0.7,
                                          rope_alpha = 0.15,
                                          rope_color = "cadetblue",
                                          normalize_height = TRUE,
                                          show_labels = TRUE,
                                          ...) {
  # save model for later use
  model <- tryCatch(
    {
      .retrieve_data(x)
    },
    error = function(e) {
      priors <- FALSE
      NULL
    }
  )


  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = model, normalize_height = normalize_height, ...)
  }

  summary <- attributes(x)$summary
  rope <- attributes(summary)$rope

  p <- ggplot2::ggplot(x, mapping = ggplot2::aes(x = .data$x, y = .data$Study, height = .data$y))

  if (!is.null(rope)) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = rope[1],
        xmax = rope[2],
        ymin = 0,
        ymax = Inf,
        fill = rope_color,
        alpha = rope_alpha
      )
  }

  insight::check_if_installed("ggridges")

  p <- p +
    ggridges::geom_ridgeline(
      mapping = ggplot2::aes(fill = .data$Group),
      color = NA,
      scale = 1,
      alpha = posteriors_alpha
    ) +
    ggplot2::geom_errorbarh(
      data = summary,
      mapping = ggplot2::aes(
        xmin = .data$CI_low,
        xmax = .data$CI_high,
        color = .data$Color
      ),
      linewidth = size_line
    ) +
    ggplot2::geom_point(
      data = summary,
      mapping = ggplot2::aes(x = .data$Estimate, color = .data$Color),
      size = size_point,
      fill = "white",
      shape = 21
    )

  p <- p +
    theme_lucid() +
    ggplot2::scale_y_discrete() +
    ggplot2::scale_fill_manual(values = c(
      "Study" = unname(metro_colors("light blue")),
      "Overall" = unname(metro_colors("amber"))
    )) +
    ggplot2::scale_colour_manual(values = c(
      "Study" = unname(metro_colors("light blue")),
      "Overall" = unname(metro_colors("amber"))
    )) +
    ggplot2::guides(fill = "none", colour = "none") +
    add_plot_attributes(x)

  if (isTRUE(show_labels)) {
    # add some space to the right panel for text
    space_factor <- sqrt(ceiling(diff(c(min(x$x), max(x$x)))) / 5)
    new_range <- pretty(c(min(x$x), max(x$x) + space_factor))

    p <- p +
      ggplot2::geom_text(
        data = summary,
        mapping = ggplot2::aes(label = .data$Estimate_CI, x = Inf),
        hjust = "inward",
        size = size_text
      ) +
      ggplot2::xlim(c(min(new_range), max(new_range))) +
      # no panel grids when we have text
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  p
}
