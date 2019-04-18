#' @rdname data_plot
#' @inheritParams data_plot
#' @examples
#'
#' # library(bayestestR)
#' # library(see)
#'
#' # data <- rnorm(1000, 1)
#' # x <- rope(data)
#' # data <- data_plot(x, data)
#' # plot(data)
#'
#' # x <- rope(data, ci=c(0.8, 0.9))
#' # data <- data_plot(x, data)
#' # plot(data)
#'
#' \dontrun{
#' library(rstanarm)
#' data <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' x <- rope(data)
#' data <- data_plot(x, data)
#' plot(data)
#'
#' x <- rope(data, ci=c(0.8, 0.9))
#' data <- data_plot(x, data)
#' plot(data)
#' }
#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.rope <- function(x, data=NULL, ...){
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  # Recontruct hdi
  hdi <- attributes(x)$HDI_area

  if("Parameter" %in% x){
    stop("Models not supported yet.")
  }
  if(length(attributes(x)$HDI_area)==1){
    hdi_area <- as.data.frame(t(unlist()))
  } else{
    hdi_area <- as.data.frame(t(as.data.frame(attributes(x)$HDI_area)))
  }
  hdi <- as.data.frame(cbind(x$CI, hdi_area))
  names(hdi) <- c("CI", "CI_low", "CI_high")
  if("Parameter" %in% x){
    hdi$Parameter <- x$Parameter
  }

  # Extract data HDI
  dataplot <- .data_plot_hdi(hdi, data)
  rope_range <- unique(c(x$ROPE_low, x$ROPE_high))
  if(length(rope_range) != 2){
    stop("Only one ROPE range accepted.")
  }

  dataplot$xmin <- rope_range[1]
  dataplot$xmax <- rope_range[2]
  dataplot$color <- ifelse(dataplot$x >= dataplot$xmin & dataplot$x <= dataplot$xmax, "Negligible", "Significant")
  attributes(dataplot)$info$rope_range <- rope_range
  attributes(dataplot)$info$title <- "Region of Practical Equivalence (ROPE)"

  class(dataplot) <- c("data_plot", "rope", "data.frame")
  dataplot
}







# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @inheritParams data_plot
#' @param rope_alpha Transparency level of ROPE ribbon.
#' @param rope_color Color of ROPE ribbon.
#' @examples
#' \dontrun{
#' # library(bayestestR)
#' # data <- rnorm(1000, 1)
#' # x <- rope(data, ci=c(0.8, 0.9))
#'
#' # plot(x, data) +
#' #   theme_modern()
#'
#' # data <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' # x <- rope(data, ci=c(0.8, 0.9))
#' }
#' @importFrom rlang .data
#' @export
plot.rope <- function(x, data=NULL, rope_alpha=0.5, rope_color="grey", ...){
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  p <- x %>%
    as.data.frame() %>%
    ggplot(aes(
      x = .data$x,
      y = .data$y,
      height = .data$height,
      group = .data$y,
      fill = .data$fill
    )) +
    ggridges::geom_ridgeline_gradient() +
    ggplot2::annotate("rect", xmin = attributes(x)$info$rope_range[1], xmax =attributes(x)$info$rope_range[2], ymin = 0, ymax = Inf, fill=rope_color, alpha = rope_alpha) +
    add_plot_attributes(x)

  p

}

