#' @export
plot.see_performance_roc <- function(x, ...) {

  if (length(unique(x$Model)) > 1) {
    p <- ggplot(
      x,
      aes(
        x = .data$Specifity,
        y = .data$Sensivity,
        colour = .data$Model
      ))
  } else {
    p <- ggplot(
      x,
      aes(
        x = .data$Specifity,
        y = .data$Sensivity,
      ))
  }


  p +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = .5) +
    geom_line() +
    ylim(c(0, 1)) +
    xlim(c(0, 1)) +
    labs(
      x = "Specifity (True Positive Rate)",
      y = "Sensivity (False Positive Rate)",
      colour = NULL
    )
}
