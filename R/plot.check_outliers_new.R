
.plot_diag_outliers_new <- function(x,
                                    na.rm = TRUE,
                                    ref.color = "darkgray",
                                    ref.linetype = "dashed",
                                    show_labels = TRUE,
                                    size_line = NULL,
                                    size_text = NULL,
                                    theme_style = theme_lucid,
                                    colors = unname(social_colors(c("green", "blue grey", "red"))),
                                    dot_alpha_level = .8,
                                    show_dots = TRUE) {
  size_line <- size_line %||% .7
  size_text <- size_text %||% 3

  plot_data <- x
  cook.levels <- attributes(x)$cook_levels
  n_params <- attributes(x)$n_params

  min_cook_level <- min(cook.levels)
  n_above <- sum(plot_data$Cooks_Distance >= min_cook_level)
  label.n <- ifelse(n_above < 5, 5, n_above)

  p <- ggplot(plot_data, aes(x = .data$Hat, .data$Std_Residuals))

  if (isTRUE(show_dots)) {
    p <- p + geom_point2(aes(colour = .data$Influential), na.rm = na.rm, alpha = dot_alpha_level)
  }

  p <- p +
    geom_vline(
      xintercept = 0,
      color = ref.color,
      linetype = ref.linetype
    ) +
    geom_hline(
      yintercept = 0,
      color = ref.color,
      linetype = ref.linetype
    ) +
    stat_smooth(
      formula = y ~ x,
      method = "loess",
      na.rm = na.rm,
      se = FALSE,
      color = colors[1]
    ) +
    scale_colour_manual(values = c("OK" = colors[2], "Influential" = colors[3])) +
    (if (isTRUE(show_labels)) {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_text_repel(
          data = plot_data[order(plot_data$Cooks_Distance, decreasing = TRUE)[1:label.n], ],
          aes(label = .data$Index, colour = .data$Influential),
          size = size_text
        )
      } else {
        geom_text(
          data = plot_data[order(plot_data$Cooks_Distance, decreasing = TRUE)[1:label.n], ],
          aes(label = .data$Index, colour = .data$Influential),
          size = size_text, position = position_nudge(
            x = diff(range(plot_data$Hat)) / 40,
            y = diff(range(plot_data$Std_Residuals)) / 20
          )
        )
      }
    }) +
    labs(
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

    .hat80 <- min(.hat) + diff(range(.hat)) * .8
    .cook_labels <- c("Cook's D = ", rep("", length(cook.levels) - 1))
    .cook_lines <- c(
      lapply(seq_along(cook.levels), function(.level) {
        annotate(
          geom = "line",
          x = .hat,
          y = .cook_ref[[.level]],
          color = colors[1],
          linetype = ref.linetype,
          size = size_line
        )
      }),
      lapply(seq_along(cook.levels), function(.level) {
        annotate(
          geom = "line",
          x = .hat,
          y = -1 * .cook_ref[[.level]],
          color = colors[1],
          linetype = ref.linetype,
          size = size_line
        )
      }),
      lapply(seq_along(cook.levels), function(.level) {
        annotate(
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
        annotate(
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
      theme_style(base_size = 10, plot.title.space = 3, axis.title.space = 5) +
      guides(colour = "none", text = "none")
  }

  p
}
