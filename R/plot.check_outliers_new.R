
.plot_diag_outliers_new <- function(
  x,
  na.rm = TRUE,
  ref.color = "darkgray",
  ref.linetype = "dashed",
  smooth.color = "blue") {

  plot_data <- x
  cook.levels <- attributes(x)$cook_levels
  n_params <- attributes(x)$n_params

  min_cook_level <- min(cook.levels)
  n_above <- sum(plot_data$Cooks_Distance >= min_cook_level)
  label.n <- ifelse(n_above < 5, 5, n_above)

  p <- ggplot(plot_data, aes(x = .data$Hat, .data$Std_Residuals)) +
    geom_point(na.rm = na.rm) +
    stat_smooth(formula = y ~ x,
                method = "loess",
                na.rm = na.rm,
                se = FALSE,
                color = smooth.color) +
    geom_vline(xintercept = 0,
               color = ref.color,
               linetype = ref.linetype) +
    geom_hline(yintercept = 0,
               color = ref.color,
               linetype = ref.linetype) +
    labs(x = expression("Leverage (" * h[ii] * ")"),
         y = "Std. Residuals",
         title = "Influential Observations",
         subtitle = "Points should be inside the contour lines") +
    ggrepel::geom_text_repel(
      data = plot_data[order(plot_data$Cooks_Distance, decreasing = TRUE)[1:label.n], ],
      aes(label = .data$Index),
      size = 2)

  if (length(cook.levels)) {
    .hat <- sort(plot_data$Hat)
    .cook_ref <- lapply(cook.levels, function(crit) {
      sqrt(crit * n_params * (1 - .hat) / .hat)
    })

    .hat80 <- min(.hat) + diff(range(.hat)) * .8
    .cook_labels <- c("Cook's D = ", rep("", length(cook.levels) - 1))
    .cook_lines <- c(
      lapply(1:length(cook.levels), function(.level) {
        annotate(
          geom = "line",
          x = .hat,
          y = .cook_ref[[.level]],
          color = ref.color,
          linetype = ref.linetype
        )
      }),
      lapply(1:length(cook.levels), function(.level) {
        annotate(
          geom = "line",
          x = .hat,
          y = -1 * .cook_ref[[.level]],
          color = ref.color,
          linetype = ref.linetype
        )
      }),
      lapply(1:length(cook.levels), function(.level) {
        annotate(
          geom = ggrepel::GeomLabelRepel,
          label = insight::format_value(cook.levels[.level], digits = 1),
          x = .hat80,
          y = sqrt(cook.levels[.level] * n_params * (1 - .hat80) / .hat80),
          hjust = "right",
          color = ref.color,
          label.size = 0,
          fill = NA,
          size = 3
        )
      }),
      lapply(1:length(cook.levels), function(.level) {
        annotate(
          geom = ggrepel::GeomLabelRepel,
          label = insight::format_value(cook.levels[.level], digits = 1),
          x = .hat80,
          y = -1 * sqrt(cook.levels[.level] * n_params * (1 - .hat80) / .hat80),
          color = ref.color,
          hjust = "right",
          label.size = 0,
          fill = NA,
          size = 3
        )
      })
    )

    p <- p + .cook_lines + theme_lucid(base_size = 10, plot.title.space = 3, axis.title.space = 5)
  }

  p
}
