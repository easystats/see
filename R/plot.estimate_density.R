#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.estimate_density <- function(x, ...) {
  dataplot <- x

  if (!"Parameter" %in% names(dataplot)) {
    dataplot$Parameter <- "Distribution"
  }

  attr(dataplot, "info") <- list("xlab" = "Values",
                                 "ylab" = "Density",
                                 "legend_fill" = "Parameter",
                                 "legend_color" = "Parameter",
                                 "title" = "Estimated Density Function")

  class(dataplot) <- c("data_plot", "see_estimate_density", class(dataplot))
  dataplot
}





# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @param stack Logical, if \code{TRUE}, densities are plotted as stacked lines.
#'   Else, densities are plotted for each parameter among each other.
#' @importFrom rlang .data
#' @export
plot.see_estimate_density <- function(x, stack = TRUE, show_intercept = FALSE, ...){
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, ...)
  }

  x <- .remove_intercept(x, show_intercept = show_intercept)

  if (stack == TRUE) {
    p <- x %>%
      ggplot(aes(
        x = .data$x,
        y = .data$y,
        color = .data$Parameter
      )) +
      geom_line() +
      add_plot_attributes(x)
  } else {
    p <- x %>%
      ggplot(aes(
        x = .data$x,
        y = .data$Parameter,
        height = .data$y
      )) +
      ggridges::geom_ridgeline_gradient() +
      add_plot_attributes(x)
  }

  p
}

