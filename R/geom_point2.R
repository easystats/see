#' @title Better looking points
#' @name geom_point2
#'
#' @description
#' The `*_borderless` geoms (and their shortcuts ending in `2`, such as `geom_point2`
#' or `geom_point_borderless`) render points without an outline stroke by default.
#' This prevents harsh edges and yields a smoother, cleaner look, especially when
#' using transparency.
#'
#' In contrast, the `*_halo` variants feature a border that automatically matches
#' the plot's background color. This creates a subtle visual separation (a "halo"
#' effect) that keeps overlapping points distinct.
#'
#' @param size Size of points.
#' @param stroke Stroke thickness.
#' @param shape Shape of points.
#' @param ... Other arguments to be passed to [ggplot2::geom_point()],
#' [ggplot2::geom_jitter()], [ggplot2::geom_pointrange()], or
#' [ggplot2::geom_count()].
#'
#' @note The color aesthetics for the `*_borderless()` and `*_halo()` functions
#' are `"fill"`, not `"color"`. See 'Examples'.
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
#' ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point_borderless(size = 4) +
#'   theme_modern()
#'
#' theme_set(theme_abyss())
#' ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point_borderless(size = 4)
#'
#' # add "halo" effect - note that the aesthetics is "fill", not "color"
#' theme_set(theme_abyss())
#' ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, fill = Species)) +
#'   geom_point_halo(size = 12)
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
geom_point_borderless <- geom_point2


#' @rdname geom_point2
#' @export
geom_jitter_borderless <- geom_jitter2


#' @rdname geom_point2
#' @export
geom_pointrange_borderless <- geom_pointrange2


#' @rdname geom_point2
#' @export
geom_point_halo <- function(...) {
  fun <- ggplot2::geom_point
  .geom_halo(fun, ...)
}


#' @rdname geom_point2
#' @export
geom_jitter_halo <- function(...) {
  fun <- ggplot2::geom_jitter
  .geom_halo(fun, ...)
}


#' @rdname geom_point2
#' @export
geom_count_halo <- function(...) {
  fun <- ggplot2::geom_count
  .geom_halo(fun, ...)
}


#' @rdname geom_point2
#' @export
geom_pointrange_halo <- function(...) {
  fun <- ggplot2::geom_pointrange
  .geom_halo(fun, ...)
}


.geom_halo <- function(fun, ...) {
  dots <- list(...)
  if (!is.null(dots$color)) {
    dots$fill <- dots$color
  }
  if (!is.null(dots$colour)) {
    dots$fill <- dots$colour
  }

  dots$colour <- dots$shape <- dots$pch <- NULL
  dots$color <- .get_theme_bg_color()

  fun_args <- c(list(pch = 21), dots)
  do.call(fun, fun_args)
}


.get_theme_bg_color <- function() {
  current_theme <- ggplot2::theme_get()

  bg_color <- ifelse(
    is.null(current_theme$panel.background$fill),
    "white",
    current_theme$panel.background$fill
  )

  bg_color
}
