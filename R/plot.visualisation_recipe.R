#' @export
plot.visualisation_recipe <- function(x, ...) {
  insight::check_if_installed("ggplot2")

  ggplot2::ggplot(data = attributes(x)$data) +
    geoms_from_list(x, ...)
}
