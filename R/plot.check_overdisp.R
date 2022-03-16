#' Plot method to check model for overdispersion
#'
#' The `plot()` method for the `performance::check_overdispersion()`
#' function.
#'
#' @param size_line Numeric value specifying size of line geoms.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' if (require("glmmTMB")) {
#'   data(Salamanders)
#'   m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#'   result <- check_overdispersion(m)
#'   plot(result)
#' }
#' @export
plot.see_check_overdisp <- function(x,
                                    size_line = .8,
                                    colors = c("#3aaf85", "#1b6ca8"),
                                    ...) {
  .plot_diag_overdispersion(
    x,
    style = theme_lucid,
    colors = colors,
    size_line = size_line
  )
}
