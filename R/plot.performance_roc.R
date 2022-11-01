#' Plot method for ROC curves
#'
#' The `plot()` method for the `performance::performance_roc()`
#' function.
#'
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' data(iris)
#' set.seed(123)
#' iris$y <- rbinom(nrow(iris), size = 1, .3)
#'
#' folds <- sample(nrow(iris), size = nrow(iris) / 8, replace = FALSE)
#' test_data <- iris[folds, ]
#' train_data <- iris[-folds, ]
#'
#' model <- glm(y ~ Sepal.Length + Sepal.Width, data = train_data, family = "binomial")
#' result <- performance_roc(model, new_data = test_data)
#' result
#' plot(result)
#' @export
plot.see_performance_roc <- function(x, ...) {
  if (length(unique(x$Model)) > 1) {
    p <- ggplot(
      x,
      aes(
        x = .data$Specificity,
        y = .data$Sensitivity,
        colour = .data$Model
      )
    )
  } else {
    p <- ggplot(
      x,
      aes(
        x = .data$Specificity,
        y = .data$Sensitivity,
      )
    )
  }


  p +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_line() +
    ylim(c(0, 1)) +
    xlim(c(0, 1)) +
    labs(
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)",
      colour = NULL
    )
}
