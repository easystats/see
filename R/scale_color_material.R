#' Material design color palette
#'
#' The palette based on material design colors (https://www.materialui.co/color).
#' Use \code{scale_color_material_d} for \emph{discrete} categories and
#' \code{scale_color_material_c} for a \emph{continuous} scale.
#'
#' @inheritParams palette_material
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE.
#'
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_material_d()
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_material_d(palette = "ice")
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_material_c(palette = "rainbow")
#'
#' @export
scale_color_material <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  pal <- palette_material(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("material_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_material
#' @export
scale_color_material_d <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  scale_color_material(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_material
#' @export
scale_color_material_c <- function(palette = "contrast", discrete = FALSE, reverse = FALSE, ...) {
  scale_color_material(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_material
#' @export
scale_colour_material <- scale_color_material

#' @rdname scale_color_material
#' @export
scale_colour_material_c <- scale_color_material_c

#' @rdname scale_color_material
#' @export
scale_colour_material_d <- scale_color_material_d





# Fill --------------------------------------------------------------------



#' @rdname scale_color_material
#' @export
scale_fill_material <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  pal <- palette_material(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("material_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' @rdname scale_color_material
#' @export
scale_fill_material_d <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  scale_fill_material(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_material
#' @export
scale_fill_material_c <- function(palette = "contrast", discrete = FALSE, reverse = FALSE, ...) {
  scale_fill_material(palette = palette, discrete = discrete, reverse = reverse, ...)
}