#' @importFrom effectsize change_scale
#' @export
data_plot.compare_performance <- function(x, data = NULL, ...){
  x <- effectsize::change_scale(x, exclude = c("Model", "Type"), to = c(.1, 1))
  x$Type <- NULL

  # recode some indices, so higher values = better fit
  for (i in c("AIC", "BIC", "RMSE")) {
    if (i %in% colnames(x)) {
      x[[i]] <- 1 - x[[i]]
    }
  }

  x <- .reshape_to_long(x, names_to = "name", columns = 2:ncol(x))
  x$name <- factor(x$name, levels = unique(x$name))

  dataplot <- as.data.frame(x)
  attr(dataplot, "info") <- list(
    "xlab" = "",
    "ylab" = "",
    "title" = "Comparison of Model Indices"
  )

  class(dataplot) <- c("data_plot", "see_compare_performance", "data.frame")
  dataplot
}




# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @importFrom rlang .data
#' @export
plot.see_compare_performance <- function(x, size = 1, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  p <- ggplot(x, aes(
    x = .data$name,
    y = .data$values,
    colour = .data$Model,
    group = .data$Model,
    fill = .data$Model
  )) +
    geom_polygon(size = size, alpha = .1) +
    coord_radar() +
    scale_y_continuous(limits = c(0, 1), labels = NULL) +
    add_plot_attributes(x)

  p
}
