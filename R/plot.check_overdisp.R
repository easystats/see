#' Plot method to check model for overdispersion
#'
#' The `plot()` method for the `performance::check_overdispersion()`
#' function.
#'
#' @param type Character vector, indicating the type of plot.
#' @param size_line Numeric value specifying size of line geoms.
#' @param dot_alpha,alpha Numeric value specifying alpha level of the
#'   confidence bands and point-geoms.
#' @param colors Character vector of length two, indicating the colors (in
#'   hex-format) for points and line.
#' @param detrend Logical that decides if the plot should be detrended.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' m <<- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- check_normality(m)
#' plot(result)
#' @export
plot.see_check_overdisp <- function(x, ...) {

}
