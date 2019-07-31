#' Material design color palette
#'
#' The palette based on material design colors (https://www.materialui.co/color).
#' Use \code{scale_color_material_d()} for \emph{discrete} categories and
#' \code{scale_color_material_c()} for a \emph{continuous} scale.
#'
#' @inheritParams palette_material
#' @inheritParams scale_color_flat
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





# Palette --------------------------------------------------------------------




# The palette based on material design colors: https://www.materialui.co/colors
material_colors_list <- c(
  `red` = "#f44336",
  `pink` = "#E91E63",
  `purple` = "#9C27B0",
  `deep purple` = "#673AB7",
  `indigo` = "#3F51B5",
  `blue` = "#2196F3",
  `light blue` = "#03A9F4",
  `cyan` = "#00BCD4",
  `teal` = "#009688",
  `green` = "#4CAF50",
  `light green` = "#8BC34A",
  `lime` = "#CDDC39",
  `yellow` = "#FFEB3B",
  `amber` = "#FFC107",
  `orange` = "#FF9800",
  `deep orange` = "#FF5722",
  `brown` = "#795548",
  `grey` = "#9E9E9E",
  `blue grey` = "#607D8B"
)


#' Extract material design colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the material design color palette. Use \code{material_colors()} to see all available color.
#'
#' @inheritParams flat_colors
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' material_colors()
#'
#' material_colors("indigo", "lime")
#'
#' @export
material_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(material_colors_list)
  }

  material_colors_list[cols]
}




material_palettes <- list(
  `full`  = material_colors(),
  `ice`  = material_colors("purple", "deep purple", "indigo", "blue", "light blue"),
  `rainbow` = material_colors("purple", "deep purple", "indigo", "blue", "light blue", "green", "light green", "lime", "amber", "orange", "deep orange" ,"red", "pink"),
  `contrast` = material_colors("blue", "green", "amber", "purple", "red"),
  `complement` = material_colors("blue", "yellow", "green", "red", "teal", "blue grey", "amber")
)






#' Material design color palette
#'
#' The palette based on material design colors (https://www.materialui.co/colors).
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   \code{\link[=scale_color_material]{scale_color_material()}}.
#'
#' @importFrom grDevices colorRampPalette
#' @export
palette_material <- function(palette = "contrast", reverse = FALSE, ...) {
  pal <- material_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


