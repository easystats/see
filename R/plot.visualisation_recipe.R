#' @export
plot.see_visualisation_recipe <- function(x, ...) {
  ggplot2::ggplot(data = attributes(x)$data) +
    geoms_from_list(x, ...)
}
