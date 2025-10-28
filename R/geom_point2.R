#' Better looking points
#'
#' Somewhat nicer points (especially in case of transparency) without outline
#' strokes (borders, contours) by default.
#'
#' @param size Size of points.
#' @param stroke Stroke thickness.
#' @param shape Shape of points.
#' @param ... Other arguments to be passed to
#'   [ggplot2::geom_point()],
#'   [ggplot2::geom_jitter()],
#'   [ggplot2::geom_pointrange()], or
#'   [ggplot2::geom_count()].
#'
#' @note The color aesthetics for `geom_point_borderless()` is `"fill"`, not
#'   `"color"`. See 'Examples'.
#'
#' @examplesIf requireNamespace("patchwork", quietly = TRUE)
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
#'
#' ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, fill = Species)) +
#'   geom_point_borderless(size = 4) +
#'   theme_modern()
#'
#' theme_set(theme_abyss())
#' ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, fill = Species)) +
#'   geom_point_borderless(size = 4)
#' @export
geom_point2 <- function(..., stroke = 0, shape = 16) {
  geom_point(stroke = stroke, shape = shape, ...)
}

#' @rdname geom_point2
#' @export
geom_jitter2 <- function(..., size = 2, stroke = 0, shape = 16) {
  geom_jitter(size = size, stroke = stroke, shape = shape, ...)
}

#' @rdname geom_point2
#' @export
geom_pointrange2 <- function(..., stroke = 0) {
  geom_pointrange(stroke = stroke, ...)
}

#' @rdname geom_point2
#' @export
geom_count2 <- function(..., stroke = 0) {
  geom_count(stroke = stroke, ...)
}

#' @rdname geom_point2
#' @export
geom_count_borderless <- geom_count2


#' @rdname geom_point2
#' @export
geom_point_borderless <- function(...) {
  geom_point(pch = 21, color = .get_theme_bg_color(), ...)
}


#' @rdname geom_point2
#' @export
geom_jitter_borderless <- function(...) {
  geom_jitter(pch = 21, color = .get_theme_bg_color(), ...)
}


#' @rdname geom_point2
#' @export
geom_pointrange_borderless <- function(...) {
  geom_pointrange(pch = 21, color = .get_theme_bg_color(), ...)
}


.get_theme_bg_color <- function() {
  current_theme <- ggplot2::theme_get()

  if (is.null(current_theme$panel.grid.major)) {
    current_theme$panel.grid.major <- current_theme$panel.grid
  }

  bg_color <- ifelse(
    is.null(current_theme$panel.grid.major$colour),
    "white",
    current_theme$panel.grid.major$colour
  )

  bg_color
}
