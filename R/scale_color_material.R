#' Material design color palette
#'
#' The palette based on [material design
#' colors](https://materialui.co/color). Use `scale_color_material_d()` for
#' *discrete* categories and `scale_color_material_c()` for a *continuous*
#' scale.
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
#' @export
scale_color_material <- function(palette = "contrast",
                                 discrete = TRUE,
                                 reverse = FALSE,
                                 aesthetics = "color",
                                 ...) {
  pal <- palette_material(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_material
#' @export
scale_color_material_d <- function(palette = "contrast",
                                   discrete = TRUE,
                                   reverse = FALSE,
                                   aesthetics = "color",
                                   ...) {
  scale_color_material(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_material
#' @export
scale_color_material_c <- function(palette = "contrast",
                                   discrete = FALSE,
                                   reverse = FALSE,
                                   aesthetics = "color",
                                   ...) {
  scale_color_material(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
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
scale_fill_material <- function(palette = "contrast",
                                discrete = TRUE,
                                reverse = FALSE,
                                aesthetics = "fill",
                                ...) {
  pal <- palette_material(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_material
#' @export
scale_fill_material_d <- function(palette = "contrast",
                                  discrete = TRUE,
                                  reverse = FALSE,
                                  aesthetics = "fill",
                                  ...) {
  scale_fill_material(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_material
#' @export
scale_fill_material_c <- function(palette = "contrast",
                                  discrete = FALSE,
                                  reverse = FALSE,
                                  aesthetics = "fill",
                                  ...) {
  scale_fill_material(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
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
#' Can be used to get the hex code of specific colors from the material design
#' color palette. Use `material_colors()` to see all available colors.
#'
#' @inheritParams flat_colors
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' material_colors()
#'
#' material_colors("indigo", "lime")
#' @export
material_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(material_colors_list)
  }

  material_colors_list[cols]
}




material_palettes <- list(
  `full` = material_colors(),
  `ice` = material_colors("purple", "deep purple", "indigo", "blue", "light blue"),
  `rainbow` = material_colors(
    "purple",
    "deep purple",
    "indigo",
    "blue",
    "light blue",
    "green",
    "light green",
    "lime",
    "amber",
    "orange",
    "deep orange",
    "red",
    "pink"
  ),
  `contrast` = material_colors("blue", "green", "amber", "purple", "red"),
  `light` = material_colors("light blue", "pink", "yellow", "light green", "orange"),
  `complement` = material_colors(
    "blue",
    "blue grey",
    "teal",
    "green",
    "light green",
    "yellow",
    "amber",
    "red"
  )
)






#' Material design color palette
#'
#' The palette based on [material design
#' colors](https://materialui.co/color).
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_material()`][scale_color_material].
#'
#' @export
palette_material <- function(palette = "contrast", reverse = FALSE, ...) {
  .retrieve_palette(palette, material_palettes, reverse = reverse, ...)
}
