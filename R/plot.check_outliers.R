#' @export
plot.see_check_outliers <- function(x, ...) {
  .plot_diag_outliers(x)
}

.plot_diag_outliers <- function(x) {
  x$.id <- 1:nrow(x)
  x$.id[!x$.outliers] <- NA
  threshold <- attr(x, "threshold", exact = TRUE)

  ggplot(x, aes(x = .data$.distance, fill = .data$.outliers, label = .data$.id)) +
    geom_histogram() +
    labs(
      title = "Check for Influential Observations",
      x = "Cook's Distance Values",
      y = NULL,
      fill = NULL
    ) +
    scale_fill_manual(values = c("#2c3e50", "#c0392b")) +
    geom_text(y = 2.5) +
    geom_vline(
      xintercept = threshold,
      linetype = "dashed",
      color = "#c0392b"
    ) +
    guides(fill = FALSE, color = FALSE, label = FALSE)
}