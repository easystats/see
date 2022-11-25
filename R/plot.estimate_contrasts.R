#' @export
data_plot.estimate_contrasts <- function(x, data = NULL, ...) {
  .data_plot_estimate_contrasts(x, data)
}


#' @keywords internal
.data_plot_estimate_contrasts <- function(x, means = NULL, ...) {
  if (is.null(means)) {
    stop("Please provide the estimated means data obtained via 'estimate_means()'.", call. = FALSE)
  } else {
    x_name <- names(means)[1]
  }

  y_name <- c("Median", "Mean", "MAP", "Coefficient")[c("Median", "Mean", "MAP", "Coefficient") %in% names(means)][1]
  dataplot <- .data_contrasts_and_means(x, means, x_name = x_name, y_name = y_name)

  attr(dataplot, "info") <- list(
    "xlab" = x_name,
    "ylab" = y_name,
    "title" = paste0("Estimated ", y_name, "s and Contrasts")
  )

  class(dataplot) <- c("data_plot", "see_estimate_contrasts", class(dataplot))
  dataplot
}




#' @keywords internal
.data_contrasts_and_means <- function(contrasts, means, x_name, y_name) {
  polygons <- contrasts
  polygons$group <- 1:nrow(polygons)

  data_means <- means
  data_means$x <- data_means[, x_name]
  data_means$y <- data_means[, y_name]
  data_means$Level2 <- data_means$Level1 <- data_means[, x_name]
  data_means$Mean2 <- data_means$Mean1 <- data_means[, y_name]
  data_means$ymin <- data_means$CI_low
  data_means$ymax <- data_means$CI_high

  polygons <- merge(polygons, data_means[c("Level1", "Mean1")], by = "Level1")
  polygons <- merge(polygons, data_means[c("Level2", "Mean2")], by = "Level2")

  polygons <- rbind(
    cbind(polygons, data.frame("x" = polygons$Level1, "y" = polygons$Mean1)),
    cbind(polygons, data.frame("x" = polygons$Level2, "y" = polygons$Mean1 - polygons$CI_low)),
    cbind(polygons, data.frame("x" = polygons$Level2, "y" = polygons$Mean1 - polygons$CI_high))
  )

  list(
    geom_polygon = polygons,
    geom_pointrange = data_means
  )
}



# Plot --------------------------------------------------------------------

#' Plot method for estimating contrasts
#'
#' The `plot()` method for the `modelbased::estimate_contrasts()`
#' function.
#'
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examplesIf require("modelbased") && require("rstanarm") && require("emmeans")
#' \donttest{
#' model <- stan_glm(Sepal.Width ~ Species, data = iris, refresh = 0)
#' contrasts <- estimate_contrasts(model)
#' means <- estimate_means(model)
#' plot(contrasts, means)
#' }
#' @importFrom ggplot2 .data
#' @export
plot.see_estimate_contrasts <- function(x, data = NULL, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  p <- ggplot() +
    geom_polygon(
      data = x$geom_polygon,
      aes(x = .data$x, y = .data$y, group = .data$group),
      alpha = 0.1
    ) +
    geom_pointrange(
      data = x$geom_pointrange,
      aes(x = .data$x, y = .data$y, ymax = .data$ymax, ymin = .data$ymin),
      color = "black"
    ) +
    add_plot_attributes(x)

  p
}
