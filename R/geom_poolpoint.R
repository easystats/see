#' Pool ball points
#'
#' Points labelled with the observation name.
#'
#' @param label Label to add inside the points.
#' @param size_text Size of text.
#' @param size_background Size of the white background circle.
#' @param size_point Size of the ball.
#' @param jitter Width and height of position jitter.
#' @param ... Other arguments to be passed to `geom_point`.
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
#'   geom_poolpoint(label = rownames(iris)) +
#'   scale_color_flat_d() +
#'   theme_modern()
#'
#'
#' ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
#'   geom_pooljitter(label = rownames(iris)) +
#'   scale_color_flat_d() +
#'   theme_modern()
#' @export
geom_poolpoint <- function(label,
                           size_text = 3.88,
                           size_background = size_text * 2,
                           size_point = size_text * 3.5,
                           ...) {
  list(
    geom_point2(size = size_point, ...),
    geom_point2(color = "white", size = size_background, ...),
    geom_text(label = label, size = size_text, ...)
  )
}


#' @rdname geom_poolpoint
#' @export
geom_pooljitter <- function(label,
                            size_text = 3.88,
                            size_background = size_text * 2,
                            size_point = size_text * 3.5,
                            jitter = 0.1,
                            ...) {
  jitter <- ggplot2::position_jitter(width = jitter, height = jitter)

  list(
    geom_point2(size = size_point, position = jitter, ...),
    geom_point2(color = "white", size = size_background, position = jitter, ...),
    geom_text(label = label, size = size_text, position = jitter, ...)
  )
}
