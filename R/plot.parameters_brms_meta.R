#' @importFrom insight format_ci
#' @importFrom bayestestR estimate_density
#' @importFrom effectsize change_scale
#' @export
data_plot.parameters_brms_meta <- function(x, data = NULL, ...) {
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
  dataplot$y <- effectsize::change_scale(dataplot$y, to = c(0, .9))

  # summary
  summary <- x[, 1:6]
  summary$Parameter <- attributes(x)$cleaned_parameters
  colnames(summary)[2] <- "Estimate"
  summary$Estimate_CI <- sprintf("%.2f %s", summary$Estimate, insight::format_ci(summary$CI_low, summary$CI_high, ci = NULL, digits = 2, width = "auto"))

  summary$Parameter <- factor(summary$Parameter, levels = rev(unique(summary$Parameter)))
  colnames(summary)[match("Parameter", colnames(summary))] <- "Study"

  summary$x <- as.numeric(NA)
  summary$y <- as.numeric(NA)
  summary$Color <- "Errorbar"

  if ("ROPE_low" %in% names(x) && "ROPE_high" %in% names(x)) {
    attr(summary, "rope") <- c(x$ROPE_low[1], x$ROPE_high[1])
  }

  dataplot <- dataplot[dataplot$Study != "tau", ]
  summary <- summary[summary$Study != "tau", ]

  dataplot$Group <- "Study"
  dataplot$Group[dataplot$Study == "Overall"] <- "Overall"
  dataplot$Color <- "Errorbar"

  attr(dataplot, "summary") <- summary
  attr(dataplot, "info") <- list("xlab" = "Standardized Mean Difference",
                                 "ylab" = "Study",
                                 "legend_fill" = NULL,
                                 "legend_color" = NULL,
                                 "title" = "Bayesian Meta-Analysis")

  class(dataplot) <- unique(c("data_plot", "see_parameters_brms_meta", class(dataplot)))
  dataplot
}




# Plot --------------------------------------------------------------------

#' Plot method for Model Parameters from Bayesian Meta-Analysis
#'
#' The \code{plot()} method for the \code{parameters::model_parameters()} function when used with brms-meta-analysis models.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_rope
#'
#' @return A ggplot2-object.
#'
#' @details
#' \subsection{Colors of density areas and errorbars}{
#'   To change the colors of the density areas, use \code{scale_fill_manual()}
#'   with named color-values, e.g. \code{scale_fill_manual(values = c("Study" = "blue", "Overall" = "green"))}.
#'   To change the color of the error bars, use \code{scale_color_manual(values = c("Errorbar" = "red"))}.
#' }
#' \subsection{Show or hide estimates and CI}{
#'   Use \code{text_size = NULL} or \code{text_size = NA} to hide the textual output of estimates and credible intervals.
#' }
#'
#' @examples
#' \dontrun{
#' if (require("bayestestR") && require("brms")) {
#'   # download data from:
#'   # https://github.com/MathiasHarrer/Doing-Meta-Analysis-in-R/blob/master/_data/Meta_Analysis_Data.RData
#'   set.seed(123)
#'   priors <- c(prior(normal(0,1), class = Intercept),
#'   prior(cauchy(0,0.5), class = sd))
#'
#'   model <- brm(
#'     TE | se(seTE) ~ 1 + (1|Author),
#'     data = Meta_Analysis_Data,
#'     prior = priors,
#'     iter = 4000
#'   )
#'
#'   mp <- model_parameters(model)
#'   plot(mp)
#' }
#' }
#' @importFrom ggridges geom_ridgeline
#' @export
plot.see_parameters_brms_meta <- function(x, point_size = 1.5, size = 0.8, text_size = 3.5, rope_alpha = 0.15, rope_color = "cadetblue", ...) {
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


  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = model, ...)
  }

  summary <- attributes(x)$summary
  rope <- attributes(summary)$rope

  p <- ggplot(x, mapping = aes(x = .data$x, y = .data$Study, height = .data$y))

  if (!is.null(rope)) {
    p <- p +
      annotate(
        "rect",
        xmin = rope[1],
        xmax = rope[2],
        ymin = 0,
        ymax = Inf,
        fill = rope_color,
        alpha = rope_alpha
      )
  }

  p <- p +
    ggridges::geom_ridgeline(mapping = aes(fill = .data$Group), color = NA, scale = 1, alpha = 0.7) +
    geom_errorbarh(data = summary, mapping = aes(xmin = .data$CI_low, xmax = .data$CI_high, color = .data$Color), size = size, alpha = 0.8) +
    geom_point(data = summary, mapping = aes(x = .data$Estimate, color = .data$Color), size = point_size, alpha = 0.8)

  if (!is.null(text_size) && !is.na(text_size)) {
    p <- p + geom_text(data = summary, mapping = aes(label = .data$Estimate_CI, x = Inf), hjust = "inward", size = text_size)
  }

  p <- p +
    theme_lucid() +
    scale_y_discrete() +
    scale_fill_manual(values = c("Study" = unname(metro_colors("light blue")), "Overall" = unname(metro_colors("amber")))) +
    scale_colour_manual(values = c("Errorbar" = unname(metro_colors("blue grey")))) +
    guides(fill = "none", colour = "none") +
    add_plot_attributes(x)

  p
}


