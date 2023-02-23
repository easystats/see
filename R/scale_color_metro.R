#' Metro color palette
#'
#' The palette based on Metro [Metro
#' colors](https://materialui.co/metrocolors).
#' Use `scale_color_metro_d` for *discrete* categories and
#' `scale_color_metro_c` for a *continuous* scale.
#'
#' @inheritParams palette_metro
#' @inheritParams scale_color_flat
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_metro_d()
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_metro_d(palette = "ice")
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_metro_c(palette = "rainbow")
#' @export
scale_color_metro <- function(palette = "complement",
                              discrete = TRUE,
                              reverse = FALSE,
                              aesthetics = "color",
                              ...) {
  pal <- palette_metro(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, paste0("metro_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_metro
#' @export
scale_color_metro_d <- function(palette = "complement",
                                discrete = TRUE,
                                reverse = FALSE,
                                aesthetics = "color",
                                ...) {
  scale_color_metro(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_metro
#' @export
scale_color_metro_c <- function(palette = "complement",
                                discrete = FALSE,
                                reverse = FALSE,
                                aesthetics = "color",
                                ...) {
  scale_color_metro(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_metro
#' @export
scale_colour_metro <- scale_color_metro

#' @rdname scale_color_metro
#' @export
scale_colour_metro_c <- scale_color_metro_c

#' @rdname scale_color_metro
#' @export
scale_colour_metro_d <- scale_color_metro_d





# Fill --------------------------------------------------------------------



#' @rdname scale_color_metro
#' @export
scale_fill_metro <- function(palette = "complement",
                             discrete = TRUE,
                             reverse = FALSE,
                             aesthetics = "fill",
                             ...) {
  pal <- palette_metro(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, paste0("metro_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_metro
#' @export
scale_fill_metro_d <- function(palette = "complement",
                               discrete = TRUE,
                               reverse = FALSE,
                               aesthetics = "fill",
                               ...) {
  scale_fill_metro(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_metro
#' @export
scale_fill_metro_c <- function(palette = "complement",
                               discrete = FALSE,
                               reverse = FALSE,
                               aesthetics = "fill",
                               ...) {
  scale_fill_metro(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}





# Palette --------------------------------------------------------------------




# The palette based on metro design colors: https://www.materialui.co/metrocolors
metro_colors_list <- c(
  `red` = "#e51400",
  `dark red` = "#a20025",
  `purple` = "#aa00ff",
  `deep purple` = "#76608a",
  `blue` = "#0050ef",
  `light blue` = "#1ba1e2",
  `teal` = "#00aba9",
  `green` = "#008a00",
  `light green` = "#60a917",
  `yellow` = "#e3c800",
  `amber` = "#f0a30a",
  `orange` = "#fa6800",
  `deep orange` = "#a0522d",
  `blue grey` = "#647687"
)


#' Extract Metro colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the Metro color
#' palette. Use `metro_colors()` to see all available colors.
#'
#' @inheritParams flat_colors
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' metro_colors()
#'
#' metro_colors("dark red", "teal")
#' @export
metro_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(metro_colors_list)
  }

  metro_colors_list[cols]
}




metro_palettes <- list(
  `full` = metro_colors(),
  `ice` = metro_colors("purple", "deep purple", "blue", "light blue"),
  `rainbow` = metro_colors(
    "purple",
    "deep purple",
    "blue",
    "light blue",
    "green",
    "light green",
    "amber",
    "orange",
    "deep orange",
    "red"
  ),
  `contrast` = metro_colors("blue", "green", "amber", "purple", "red"),
  `light` = material_colors("light blue", "red", "yellow", "light green", "orange"),
  `complement` = metro_colors(
    "blue grey",
    "blue",
    "light blue",
    "teal",
    "green",
    "yellow",
    "amber",
    "orange",
    "red"
  )
)






#' Metro color palette
#'
#' The palette based on [Metro
#' colors](https://materialui.co/metrocolors).
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_metro()`][scale_color_metro].
#'
#' @export
palette_metro <- function(palette = "complement", reverse = FALSE, ...) {
  .retrieve_palette(palette, metro_palettes, reverse = reverse, ...)
}
