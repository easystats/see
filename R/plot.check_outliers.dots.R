.plot_diag_outliers_dots <- function(
  x,
  na.rm = TRUE,
  ref.color = "darkgray",
  ref.linetype = "dashed",
  show_labels = TRUE,
  linewidth = NULL,
  size_point = 2,
  size_text = NULL,
  size_axis_title = base_size,
  size_title = 12,
  theme_style = NULL,
  base_size = 10,
  colors = unname(social_colors(c("green", "blue grey", "red"))),
  alpha_dot = 0.8,
  show_dots = TRUE,
  maximum_dots = 2000
) {
  linewidth <- linewidth %||% 0.7
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
    ) +
    ggplot2::stat_smooth(
      formula = y ~ x,
      method = "loess",
      na.rm = na.rm,
      se = FALSE,
      color = colors[1],
      linewidth = linewidth
    ) +
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
    .cook_ref <- lapply(cook.levels, function(crit) {
      sqrt(crit * n_params * (1 - .hat) / .hat)
    })

    .hat80 <- min(.hat) + diff(range(.hat)) * 0.8
    .cook_labels <- c("Cook's D = ", rep("", length(cook.levels) - 1))
    .cook_lines <- c(
      lapply(seq_along(cook.levels), function(.level) {
        ggplot2::annotate(
          geom = "line",
          x = .hat,
          y = .cook_ref[[.level]],
          color = colors[1],
          linetype = ref.linetype,
          linewidth = linewidth
        )
      }),
      lapply(seq_along(cook.levels), function(.level) {
        ggplot2::annotate(
          geom = "line",
          x = .hat,
          y = -1 * .cook_ref[[.level]],
          color = colors[1],
          linetype = ref.linetype,
          linewidth = linewidth
        )
      }),
      lapply(seq_along(cook.levels), function(.level) {
        ggplot2::annotate(
          geom = "text",
          label = insight::format_value(cook.levels[.level], digits = 1),
          x = .hat80,
          y = sqrt(cook.levels[.level] * n_params * (1 - .hat80) / .hat80),
          hjust = "right",
          vjust = -1,
          color = colors[1],
          size = size_text
        )
      }),
      lapply(seq_along(cook.levels), function(.level) {
        ggplot2::annotate(
          geom = "text",
          label = insight::format_value(cook.levels[.level], digits = 1),
          x = .hat80,
          y = -1 * sqrt(cook.levels[.level] * n_params * (1 - .hat80) / .hat80),
          color = colors[1],
          hjust = "right",
          vjust = 1.5,
          size = size_text
        )
      })
    )

    p <- p +
      .cook_lines +
      theme_style +
      ggplot2::guides(colour = "none", text = "none")
  }

  p
}

.plot_diag_outliers_dots_count <- function(
  x,
  show_labels = TRUE,
  size_text = 3.5,
  rescale_distance = TRUE
) {
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

  p + ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2))
}
