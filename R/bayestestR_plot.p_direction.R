#' @rdname data_plot
#' @inheritParams data_plot
#' @examples
#' \dontrun{
#' library(bayestestR)
#' library(see)
#'
#' data <- rnorm(1000, 1)
#' x <- p_direction(data)
#' data <- data_plot(x, data)
#' plot(data)
#'
#' library(rstanarm)
#' model <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' x <- p_direction(model)
#' data <- data_plot(x)
#' plot(data)
#' }
#' @import dplyr
#' @export
data_plot.p_direction <- function(x, data=NULL, ...){
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  data <- as.data.frame(data)
  if(ncol(data) > 1){
    levels_order <- rev(x$Parameter)
    data <- data[, x$Parameter]
    data_plot <- data.frame()
    for(i in names(data)){
      data_plot <- rbind(data_plot,
                         .compute_densities_pd(data[[i]], name=i))
    }
  } else{
    levels_order <- NULL
    data_plot <- .compute_densities_pd(data[, 1], name="Posterior")
  }

  data_plot <- data_plot %>%
    dplyr::group_by_("y", "fill") %>%
    dplyr::mutate_("n" = "n()") %>%
    dplyr::ungroup() %>%
    dplyr::group_by_("y") %>%
    dplyr::mutate_("prop" = "n / n()") %>%
    dplyr::ungroup() %>%
    dplyr::mutate_('fill2' = 'ifelse(prop >= .5, "Most probable", "Less probable")') %>%
    dplyr::select(-dplyr::one_of("n", "prop"))

  if(!is.null(levels_order)){
    data_plot$y <- factor(data_plot$y, levels=levels_order)
  }

  attr(data_plot, "info") <- list("xlab" = "Possible values",
                                  "ylab" = "Parameters",
                                  "legend_fill" = "Effect direction")

  class(data_plot) <- c("data_plot", "p_direction", class(data_plot))
  data_plot
}



#' @importFrom stats density
#' @keywords internal
.compute_densities_pd <- function(x, name="Y"){
  out <- x %>%
    density() %>%
    .as.data.frame_density() %>%
    dplyr::mutate_('fill' = 'ifelse(x < 0, "Negative", "Positive")') %>%
    dplyr::mutate_('height' = 'y',
                   'y' = 'name')

  out$height <- as.vector((out$height - min(out$height, na.rm = TRUE)) / diff(range(out$height, na.rm = TRUE), na.rm = TRUE))
  out
}



# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @inheritParams data_plot
#' @examples
#' \dontrun{
#' library(bayestestR)
#' data <- rnorm(1000, 1)
#' x <- p_direction(data)
#'
#' plot(x, data) +
#'   theme_modern()
#'
#' }
#' @export
plot.p_direction <- function(x, data=NULL, ...){
  if(!"data_plot" %in% class(x)){
    x <- data_plot(x, data=data)
  }

  p <- x %>%
    as.data.frame() %>%
    ggplot(aes_string(x="x", y="y", height="height", group="y", fill="fill")) +
    ggridges::geom_ridgeline_gradient() +
    .add_plotinfo(x) +
    geom_vline(aes(xintercept=0))

  p

}

