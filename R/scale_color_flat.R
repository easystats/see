#' Flat UI color palette
#'
#' The palette based on [Flat UI](https://materialui.co/flatuicolors).
#' Use `scale_color_flat_d` for *discrete* categories and
#' `scale_color_flat_c` for a *continuous* scale.
#'
#' @inheritParams palette_flat
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param ... Additional arguments passed to `discrete_scale()` or
#'  `scale_color_gradientn()`, used respectively when discrete is
#'  `TRUE` or `FALSE`.
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_flat_d()
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_flat_d(palette = "ice")
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_flat_c(palette = "rainbow")
#' @export
scale_color_flat <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  pal <- palette_flat(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("flat_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_flat
#' @export
scale_color_flat_d <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  scale_color_flat(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_flat
#' @export
scale_color_flat_c <- function(palette = "contrast", discrete = FALSE, reverse = FALSE, ...) {
  scale_color_flat(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_flat
#' @export
scale_colour_flat <- scale_color_flat

#' @rdname scale_color_flat
#' @export
scale_colour_flat_c <- scale_color_flat_c

#' @rdname scale_color_flat
#' @export
scale_colour_flat_d <- scale_color_flat_d





# Fill --------------------------------------------------------------------



#' @rdname scale_color_flat
#' @export
scale_fill_flat <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  pal <- palette_flat(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("flat_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' @rdname scale_color_flat
#' @export
scale_fill_flat_d <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  scale_fill_flat(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_flat
#' @export
scale_fill_flat_c <- function(palette = "contrast", discrete = FALSE, reverse = FALSE, ...) {
  scale_fill_flat(palette = palette, discrete = discrete, reverse = reverse, ...)
}





# Palette --------------------------------------------------------------------




# The palette based on flat design colors: https://www.materialui.co/flatuicolors
flat_colors_list <- c(
  `red` = "#e74c3c",
  `dark red` = "#c0392b",
  `purple` = "#9b59b6",
  `deep purple` = "#8e44ad",
  `blue` = "#2980b9",
  `light blue` = "#3498db",
  `cyan` = "#1abc9c",
  `teal` = "#16a085",
  `green` = "#27ae60",
  `light green` = "#2ecc71",
  `yellow` = "#f1c40f",
  `amber` = "#f39c12",
  `orange` = "#e67e22",
  `deep orange` = "#d35400",
  `grey` = "#95a5a6",
  `blue grey` = "#7f8c8d"
)


#' Extract Flat UI colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the Flat UI color
#' palette. Use `flat_colors()` to see all available color.
#'
#' @param ... Character names of colors.
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' flat_colors()
#'
#' flat_colors("dark red", "teal")
#' @export
flat_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(flat_colors_list)
  }

  flat_colors_list[cols]
}




flat_palettes <- list(
  `full` = flat_colors(),
  `ice` = flat_colors("purple", "deep purple", "blue", "light blue"),
  `rainbow` = flat_colors("purple", "deep purple", "blue", "light blue", "green", "light green", "amber", "orange", "deep orange", "red"),
  `contrast` = flat_colors("blue", "green", "amber", "purple", "red"),
  `light` = flat_colors("light blue", "purple", "yellow", "light green", "orange"),
  `complement` = flat_colors("blue grey", "blue", "light blue", "teal", "green", "yellow", "amber", "orange", "red")
)






#' Flat UI color palette
#'
#' The palette based on [Flat UI](https://materialui.co/flatuicolors).
#'
#' @param palette Character name of palette. Depending on the color scale, can
#'   be `"full"`, `"ice"`, `"rainbow"`, `"complement"`,
#'   `"contrast"` or `"light"` (for dark themes).
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments to pass to [`colorRampPalette()`][colorRampPalette].
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_flat()`][scale_color_flat].
#'
#' @export
palette_flat <- function(palette = "contrast", reverse = FALSE, ...) {
  pal <- flat_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
