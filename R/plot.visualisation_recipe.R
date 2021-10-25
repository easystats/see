#' @export
plot.see_visualisation_recipe <- function(x, ...) {
  if("ggraph" %in% names(attributes(x))) {

    insight::check_if_installed("ggraph")
    # This below is because ggraph must actually be loaded
    if (!requireNamespace("ggraph", quietly = TRUE)) {
    } else {
      si <- utils::sessionInfo()
      other_packages <- names(si$otherPkgs)
      if (!is.null(other_packages) && !("ggraph" %in% other_packages)) {
        insight::check_if_installed("ggraph")
        return(NULL)
      }
    }
    ggraph::ggraph(attributes(x)$data, layout = attributes(x)$layout) +
      geoms_from_list(x)

  } else {
    ggplot2::ggplot(data = attributes(x)$data) +
      geoms_from_list(x, ...)
  }

}

# Example
# vr1 <- datawizard::visualisation_recipe(correlation::correlation(iris))
# vr2 <- datawizard::visualisation_recipe(summary(correlation::correlation(iris)))
#' @export
plot.see_visualisation_recipes <- function(x, ...) {
  the_plots <- list()
  for(vr in x) {
    the_plots[paste0("p", length(the_plots) + 1)] <- plot(vr)
  }
  plots(the_plots, ...)
}

