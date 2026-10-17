.plot_diag_outliers_dots <- function(
  x,
  na.rm = TRUE,
  ref.color = "darkgray",
  ref.linetype = "dashed",
  show_labels = TRUE,
  size_line = NULL,
  size_point = 2,
  size_text = NULL,
  size_axis_title = base_size,
  size_title = 12,
  base_size = 10,
  theme = NULL,
  colors = unname(social_colors(c("green", "blue grey", "red"))),
  alpha_dot = 0.8,
  show_dots = TRUE,
  show_smooth = TRUE,
  maximum_dots = 2000
) {
  theme <- .set_default_theme(
    x,
    theme,
    base_size,
    size_axis_title,
    size_title,
    default_theme = ggplot2::theme_grey()
  )

  size_line <- size_line %||% 0.7
  size_text <- size_text %||% 3

  plot_data <- x

  if (is.null(maximum_dots)) {
    maximum_dots <- 2000
  }

  # Sample data if too large for performance (issue #420)
  # But preserve influential points for labeling
  if (nrow(plot_data) > maximum_dots) {
    # Keep all influential points
    influential_points <- plot_data[
      plot_data$Influential == "Influential",
      ,
      drop = FALSE
    ]
    non_influential_points <- plot_data[
      plot_data$Influential != "Influential",
      ,
      drop = FALSE
    ]

    # Sample from non-influential points
    if (nrow(non_influential_points) > (maximum_dots * 0.8)) {
      set.seed(123)
      sample_indices <- sample.int(
        nrow(non_influential_points),
        round((maximum_dots * 0.8)),
        replace = FALSE
      )
      non_influential_points <- non_influential_points[
        sample_indices,
        ,
        drop = FALSE
      ]
    }

    # Combine back
    plot_data <- rbind(influential_points, non_influential_points)
  }

  cook.levels <- attributes(x)$cook_levels
  n_params <- attributes(x)$n_params

  min_cook_level <- min(cook.levels, na.rm = TRUE)
  n_above <- sum(plot_data$Cooks_Distance >= min_cook_level, na.rm = TRUE)
  label.n <- pmax(n_above, 5)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Hat, .data$Std_Residuals)
  )

  if (isTRUE(show_dots)) {
    p <- p +
      geom_point2(
        ggplot2::aes(colour = .data$Influential),
        na.rm = na.rm,
        alpha = alpha_dot,
        size = size_point
      )
  } else {
    # keep the y axis on the residuals, where the contour lines are cut
    p <- p + ggplot2::geom_blank()
  }

  p <- p +
    ggplot2::geom_vline(
      xintercept = 0,
      color = ref.color,
      linetype = ref.linetype
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = ref.color,
      linetype = ref.linetype
    )

  # check if we have enough unique x-values to plot smoother, and if the user
  # did not turn the smoother off
  if (isTRUE(show_smooth) && insight::n_unique(plot_data$Hat) > 30) {
    p <- p +
      ggplot2::stat_smooth(
        formula = y ~ x,
        method = "loess",
        na.rm = na.rm,
        se = FALSE,
        color = colors[1],
        linewidth = size_line
      )
  }

  p <- p +
    ggplot2::scale_colour_manual(
      values = c(OK = colors[2], Influential = colors[3])
    ) +
    (if (isTRUE(show_labels)) {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_text_repel(
          data = plot_data[
            order(plot_data$Cooks_Distance, decreasing = TRUE)[1:label.n],
          ],
          ggplot2::aes(label = .data$Index, colour = .data$Influential),
          size = size_text
        )
      } else {
        ggplot2::geom_text(
          data = plot_data[
            order(plot_data$Cooks_Distance, decreasing = TRUE)[1:label.n],
          ],
          ggplot2::aes(label = .data$Index, colour = .data$Influential),
          size = size_text,
          position = ggplot2::position_nudge(
            x = diff(range(plot_data$Hat)) / 40,
            y = diff(range(plot_data$Std_Residuals)) / 20
          )
        )
      }
    }) +
    ggplot2::labs(
      x = expression("Leverage (" * h[ii] * ")"),
      y = "Std. Residuals",
      title = "Influential Observations",
      subtitle = "Points should be inside the contour lines",
      colour = NULL
    )

  if (length(cook.levels)) {
    .hat <- sort(plot_data$Hat)
    .hat80 <- min(.hat) + diff(range(.hat)) * 0.8

    # Contour lines are cut where they leave the range of the residuals, so
    # that the y axis follows the residuals and not the lines, which grow as
    # leverage shrinks (with the number of observations). Without finite
    # residuals, the lines are drawn in full.
    .res <- plot_data$Std_Residuals[is.finite(plot_data$Std_Residuals)]
    .bounds <- if (length(.res)) c(max(.res), -min(.res)) else c(Inf, Inf)

    .cook_ref <- function(crit, hat) sqrt(crit * n_params * (1 - hat) / hat)

    # the part of a contour line at or below `bound`, with the exact point
    # where it crosses `bound`; NULL if no part of the line is inside
    .cook_line <- function(crit, bound) {
      y <- .cook_ref(crit, .hat)
      keep <- !is.na(y) & y <= bound
      if (!any(keep)) {
        return(NULL)
      }
      x <- .hat[keep]
      y <- y[keep]
      if (is.finite(bound)) {
        h_cross <- crit * n_params / (bound^2 + crit * n_params)
        if (h_cross > min(.hat) && h_cross < x[1]) {
          x <- c(h_cross, x)
          y <- c(bound, y)
        }
      }
      # a single point draws no line
      if (length(x) < 2) {
        return(NULL)
      }
      list(x = x, y = y)
    }

    .sides <- c(upper = 1, lower = -1)
    .pieces <- list(
      upper = lapply(cook.levels, .cook_line, bound = .bounds[1]),
      lower = lapply(cook.levels, .cook_line, bound = .bounds[2])
    )

    .cook_lines <- lapply(names(.sides), function(.s) {
      lapply(.pieces[[.s]], function(.piece) {
        if (is.null(.piece)) {
          return(NULL)
        }
        ggplot2::annotate(
          geom = "line",
          x = .piece$x,
          y = .sides[[.s]] * .piece$y,
          color = colors[1],
          linetype = ref.linetype,
          linewidth = size_line
        )
      })
    })

    # labels sit at 80% of the leverage range, or at the largest leverage if
    # the cut line starts to the right of that point; the label of a cut line
    # is on the inner side, so the panel edge does not clip it
    .cook_text <- lapply(names(.sides), function(.s) {
      lapply(seq_along(cook.levels), function(.level) {
        .piece <- .pieces[[.s]][[.level]]
        if (is.null(.piece)) {
          return(NULL)
        }
        .x <- if (.piece$x[1] <= .hat80) .hat80 else max(.hat)
        .cut <- .piece$x[1] > min(.hat)
        .vjust <- if (xor(.cut, .s == "lower")) 1.5 else -1
        ggplot2::annotate(
          geom = "text",
          label = insight::format_value(cook.levels[.level], digits = 1),
          x = .x,
          y = .sides[[.s]] * .cook_ref(cook.levels[.level], .x),
          hjust = "right",
          vjust = .vjust,
          color = colors[1],
          size = size_text
        )
      })
    })

    .cook_lines <- unlist(.cook_lines, recursive = FALSE)
    .cook_text <- unlist(.cook_text, recursive = FALSE)
    .cook_lines <- Filter(Negate(is.null), .cook_lines)
    .cook_text <- Filter(Negate(is.null), .cook_text)

    # say that all points are inside only if the lines could be computed
    .computable <- length(.res) > 0 &&
      length(n_params) == 1 &&
      isTRUE(is.finite(n_params)) &&
      any(is.finite(cook.levels))
    if (.computable && !length(.cook_lines)) {
      p <- p +
        ggplot2::labs(
          subtitle = "All points are inside the contour lines (outside the plot area)"
        )
    }

    p <- p +
      .cook_lines +
      .cook_text +
      theme +
      ggplot2::guides(colour = "none", text = "none")
  }

  p
}

.plot_diag_outliers_dots_count <- function(
  x,
  show_labels = TRUE,
  size_text = 3.5,
  size_title = 12,
  base_size = 10,
  size_axis_title = base_size,
  rescale_distance = TRUE,
  theme = NULL
) {
  theme <- .set_default_theme(
    x,
    theme,
    base_size,
    size_axis_title,
    size_title,
    default_theme = ggplot2::theme_grey()
  )

  d <- data_plot(x, rescale_distance = rescale_distance)
  d$Id <- seq_len(nrow(d))
  d$Outliers <- as.factor(attr(x, "data", exact = TRUE)[["Outlier"]])
  d$Id[d$Outliers == "0"] <- NA

  method <- switch(
    attr(x, "method", exact = TRUE),
    cook = "Cook's Distance",
    pareto = "Pareto",
    mahalanobis = "Mahalanobis Distance",
    ics = "Invariant Coordinate Selection",
    mcd = "Minimum Covariance Determinant",
    optics = "OPTICS",
    iforest = "Isolation Forest",
    "Cook's Distance"
  )

  threshold <- attr(x, "threshold", exact = TRUE)[[method]]
  rescaled <- attr(d, "rescale_distance")
  if (isTRUE(rescaled)) {
    x_lab <- paste0(method, " (rescaled range 0-1)")
  } else {
    x_lab <- method
  }

  size_text <- size_text %||% 3.5

  # leave out rows without a distance (e.g., rows the model was not fitted on)
  d <- d[!is.na(d$Distance), , drop = FALSE]

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$Distance,
      fill = .data$Outliers,
      group = .data$Id,
      label = .data$Id
    )
  ) +
    ggplot2::geom_histogram() +
    ggplot2::labs(
      title = "Influential Observations",
      subtitle = "High Cook's distance might reflect potential outliers",
      x = x_lab,
      y = "Count",
      fill = NULL
    ) +
    ggplot2::scale_fill_manual(values = c("#2c3e50", "#c0392b")) +
    ggplot2::guides(fill = "none", color = "none", label = "none")

  if (!is.null(threshold) && !is.na(threshold)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = threshold,
        linetype = "dashed",
        color = "#c0392b"
      )
  }

  if (isTRUE(show_labels)) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(y = 2.5, size = size_text, na.rm = TRUE)
    } else {
      p <- p + ggplot2::geom_text(y = 2.5, size = size_text, na.rm = TRUE)
    }
  }

  p + ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2)) + theme
}
