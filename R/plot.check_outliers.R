#' @param text_size Size of text labels.
#' @rdname data_plot
#' @export
plot.see_check_outliers <- function(x, text_size = 3.5, ...) {
  .plot_diag_outliers(x, text_size)
}

.plot_diag_outliers <- function(x, text_size = 3.5) {
  x$.id <- 1:nrow(x)
  x$.id[!x$.outliers] <- NA
  threshold <- attr(x, "threshold", exact = TRUE)

  if (is.null(text_size)) text_size <- 3.5

  ggplot(x, aes(x = .data$.distance, fill = .data$.outliers, label = .data$.id)) +
    geom_histogram() +
    labs(
      title = "Check for Influential Observations",
      x = "Cook's Distance Values",
      y = "Count",
      fill = NULL
    ) +
    scale_fill_manual(values = c("#2c3e50", "#c0392b")) +
    geom_text(y = 2.5, size = text_size) +
    geom_vline(
      xintercept = threshold,
      linetype = "dashed",
      color = "#c0392b"
    ) +
    guides(fill = FALSE, color = FALSE, label = FALSE)
}
