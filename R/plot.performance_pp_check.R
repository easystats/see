#' @importFrom stats reshape
#' @export
data_plot.performance_pp_check <- function(x, ...) {
  columns <- colnames(x)
  dataplot <- stats::reshape(
    x,
    times = columns,
    timevar = "key",
    v.names = "values",
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dataplot[["values"]])) {
    dataplot[["values"]] <- as.character(dataplot[["values"]])
  }

  dataplot <- dataplot[, 1:(ncol(dataplot) - 1), drop = FALSE]
  dataplot$key[dataplot$key != "y"] <- "yrep"
  dataplot$grp <- rep(1:ncol(x), each = nrow(x))

  class(dataplot) <- unique(c("data_plot", "see_performance_pp_check", class(dataplot)))
  dataplot
}


# Plot --------------------------------------------------------------------

#' Plot method for posterior predictive checks
#'
#' The \code{plot()} method for the \code{performance::pp_check()} function.
#'
#' @param line_alpha Alpha value of lines indicating \code{yrep}.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' pp_check(model)
#' @export
print.see_performance_pp_check <- function(x, size_line = .7, line_alpha = .25, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  ggplot() +
    stat_density(data = x[x$key != "y", ], mapping = aes(x = .data$values, group = .data$grp, color = .data$key), geom = "line", position = "identity", alpha = line_alpha, size = size_line) +
    stat_density(data = x[x$key == "y", ], mapping = aes(x = .data$values, group = .data$grp, color = .data$key), geom = "line", position = "identity", size = size_line) +
    scale_color_material()
}
