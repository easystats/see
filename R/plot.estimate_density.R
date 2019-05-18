#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.estimate_density <- function(x, ...){
  dataplot <- x

  if(!"Parameter" %in% names(dataplot)){
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
#' @importFrom rlang .data
#' @export
plot.see_estimate_density <- function(x, stack = TRUE, ...){
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, ...)
  }

  if(stack == TRUE){
    p <- x %>%
      ggplot(aes(
        x = .data$x,
        y = .data$y,
        color = .data$Parameter
      )) +
      geom_line() +
      add_plot_attributes(x)
  } else{
    p <- x %>%
      ggplot(aes(
        x = .data$x,
        y = .data$Parameter,
        height = .data$y,
        fill = .data$Parameter
      )) +
      ggridges::geom_ridgeline_gradient() +
      add_plot_attributes(x)
  }

  p
}

