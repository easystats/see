#' Better looking points
#'
#' Somewhat nicer points (especially in case of transparency) without outline strokes (borders, contours) by default.
#'
#' @param size Size of points.
#' @param stroke Stroke thickness.
#' @param shape Shape of points.
#' @param ... Other arguments to be passed to \code{\link[ggplot2:geom_point]{geom_point}}, \code{\link[ggplot2:geom_jitter]{geom_jitter}}, \code{\link[ggplot2:geom_pointrange]{geom_pointrange}}, or \code{\link[ggplot2:geom_count]{ggplot2::geom_count}}.
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' normal <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
#'   geom_point(size = 8, alpha = 0.3) +
#'   theme_modern()
#'
#' new <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
#'   geom_point2(size = 8, alpha = 0.3) +
#'   theme_modern()
#'
#' plots(normal, new, n_columns = 2)
#' @export
geom_point2 <- function(..., stroke = 0, shape = 16) {
  geom_point(stroke = stroke, shape = shape, ...)
}

#' @rdname geom_point2
#' @export
geom_point_borderless <- geom_point2


#' @rdname geom_point2
#' @export
geom_jitter2 <- function(..., size = 2, stroke = 0, shape = 16) {
  geom_jitter(size = size, stroke = stroke, shape = shape, ...)
}

#' @rdname geom_point2
#' @export
geom_jitter_borderless <- geom_jitter2


#' @rdname geom_point2
#' @export
geom_pointrange2 <- function(..., stroke = 0) {
  geom_pointrange(stroke = stroke, ...)
}

#' @rdname geom_point2
#' @export
geom_pointrange_borderless <- geom_pointrange2


#' @rdname geom_point2
#' @export
geom_count2 <- function(..., stroke = 0) {
  geom_count(stroke = stroke, ...)
}

#' @rdname geom_point2
#' @export
geom_count_borderless <- geom_count2
