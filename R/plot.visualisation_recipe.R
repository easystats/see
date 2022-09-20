#' @export
plot.see_visualisation_recipe <- function(x, ...) {
  if ("ggraph" %in% names(attributes(x))) {
    insight::check_if_installed("ggraph")
    if (!"ggraph" %in% .packages()) {
      attachNamespace("ggraph") # Needs to be attached
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
# x <- list(p1 = vr1, p2 = vr2)
# class(x) <- "see_visualisation_recipes"
# plot(x)
#' @export
plot.see_visualisation_recipes <- function(x, ...) {
  the_plots <- list()
  for (i in names(x)) {
    the_plots[[i]] <- graphics::plot(x[[i]])
  }
  pw <- plots(the_plots, ...)
  .safe_print_plots(pw)
  invisible(pw)
}
