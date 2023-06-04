#' Plot method for multicollinearity checks
#'
#' The `plot()` method for the `performance::check_collinearity()` function.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- check_collinearity(m)
#' result
#' plot(result)
#' @importFrom ggplot2 .data
#' @export
plot.see_check_collinearity <- function(x,
                                        data = NULL,
                                        colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                        size_point = 4,
                                        size_line = 0.8,
                                        ...) {
  if (is.null(data)) {
    dat <- insight::compact_list(.retrieve_data(x))
  } else {
    dat <- data
  }

  if (is.null(dat)) {
    return(NULL)
  }

  dat$group <- "low"
  dat$group[dat$VIF >= 5 & dat$VIF < 10] <- "moderate"
  dat$group[dat$VIF >= 10] <- "high"

  dat <- datawizard::data_rename(
    dat,
    pattern = c("Term", "VIF", "SE_factor", "Component"),
    replacement = c("x", "y", "se", "facet")
  )

  dat <- datawizard::data_select(dat, select = c("x", "y", "facet", "group"))

  if (insight::n_unique(dat$facet) <= 1) {
    dat$facet <- NULL
  }

  .plot_diag_vif(
    dat,
    size_point = size_point,
    size_line = size_line,
    colors = colors,
    ci_data = attributes(x)$CI,
    is_check_model = FALSE
  )
}


.plot_diag_vif <- function(x,
                           size_point,
                           size_line,
                           theme_style = theme_lucid,
                           colors = unname(social_colors(c("green", "blue", "red"))),
                           ci_data = NULL,
                           is_check_model = FALSE) {
  ylim <- ceiling(max(x$y, na.rm = TRUE))
  xlim <- nrow(x)
  if (ylim < 10) ylim <- 10

  if (!is.null(ci_data)) {
    x <- cbind(x, ci_data)
  } else {
    x$VIF_CI_low <- NA_real_
    x$VIF_CI_high <- NA_real_
  }

  # make sure legend is properly sorted
  x$group <- factor(x$group, levels = c("low", "moderate", "high"))
  levels(x$group) <- c("Low (< 5)", "Moderate (< 10)", "High (\u2265 10)")
  names(colors) <- c("Low (< 5)", "Moderate (< 10)", "High (\u2265 10)")

  p <- ggplot2::ggplot(x) +
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      color = .data$group,
      ymin = .data$VIF_CI_low,
      ymax = .data$VIF_CI_high
    ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 1,
      ymax = 5,
      fill = colors[1],
      color = NA,
      alpha = 0.15
    ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 5,
      ymax = 10,
      fill = colors[2],
      color = NA,
      alpha = 0.15
    ) +
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 10,
      ymax = Inf,
      fill = colors[3],
      color = NA,
      alpha = 0.15
    ) +
    {
      if (!is.null(ci_data)) {
        list(
          ggplot2::geom_linerange(
            linewidth = size_line,
            na.rm = TRUE
          ),
          ggplot2::geom_segment(
            data = x[x$VIF_CI_high > ylim * 1.15, ],
            mapping = aes(
              x = .data$x,
              xend = .data$x,
              y = .data$y,
              yend = .data$VIF_CI_high
            ),
            lineend = "round",
            linejoin = "round",
            arrow = ggplot2::arrow(
              ends = "last", type = "closed",
              angle = 20, length = ggplot2::unit(0.03, "native")
            ),
            show.legend = FALSE
          )
        )
      }
    } +
    geom_point2(
      size = size_point,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      title = "Collinearity",
      subtitle = "High collinearity (VIF) may inflate parameter uncertainty",
      x = NULL,
      y = paste("Variance Inflation", "Factor (VIF, log-scaled)", sep = ifelse(is_check_model, "\n", " "))
    ) +
    ggplot2::scale_color_manual(
      values = colors,
      aesthetics = c("color", "fill"),
      guide = ggplot2::guide_legend(title = NULL)
    ) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    ) +
    ggplot2::scale_y_continuous(
      limits = c(1, ylim * 1.15),
      oob = scales::oob_squish,
      trans = "log10",
      expand = c(0, 0),
      breaks = scales::log_breaks(n = 7, base = 10)
    ) +
    ggplot2::scale_x_discrete() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(-5, -5, -5, -5)
    )

  if ("facet" %in% colnames(x)) {
    p <- p + ggplot2::facet_wrap(~facet, nrow = 1, scales = "free")
  }

  p
}
