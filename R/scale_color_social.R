#' Social color palette
#'
#' The palette based [Social colors](https://materialui.co/socialcolors).
#' Use `scale_color_social_d` for *discrete* categories and
#' `scale_color_social_c` for a *continuous* scale.
#'
#' @inheritParams palette_social
#' @inheritParams scale_color_flat
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_social_d()
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_social_d(palette = "ice")
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_social_c(palette = "rainbow")
#' @export
scale_color_social <- function(palette = "complement", discrete = TRUE, reverse = FALSE, aesthetics = "color", ...) {
  pal <- palette_social(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


# Aliases -----------------------------------------------------------------


#' @rdname scale_color_social
#' @export
scale_color_social_d <- function(palette = "complement", discrete = TRUE, reverse = FALSE, aesthetics = "color", ...) {
  scale_color_social(palette = palette, discrete = discrete, reverse = reverse, aesthetics = aesthetics, ...)
}

#' @rdname scale_color_social
#' @export
scale_color_social_c <- function(palette = "complement", discrete = FALSE, reverse = FALSE, aesthetics = "color", ...) {
  scale_color_social(palette = palette, discrete = discrete, reverse = reverse, aesthetics = aesthetics, ...)
}

#' @rdname scale_color_social
#' @export
scale_colour_social <- scale_color_social

#' @rdname scale_color_social
#' @export
scale_colour_social_c <- scale_color_social_c

#' @rdname scale_color_social
#' @export
scale_colour_social_d <- scale_color_social_d


# Fill --------------------------------------------------------------------


#' @rdname scale_color_social
#' @export
scale_fill_social <- function(palette = "complement", discrete = TRUE, reverse = FALSE, aesthetics = "fill", ...) {
  pal <- palette_social(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_social
#' @export
scale_fill_social_d <- function(palette = "complement", discrete = TRUE, reverse = FALSE, aesthetics = "fill", ...) {
  scale_fill_social(palette = palette, discrete = discrete, reverse = reverse, aesthetics = aesthetics, ...)
}

#' @rdname scale_color_social
#' @export
scale_fill_social_c <- function(palette = "complement", discrete = FALSE, reverse = FALSE, aesthetics = "fill", ...) {
  scale_fill_social(palette = palette, discrete = discrete, reverse = reverse, aesthetics = aesthetics, ...)
}


# Palette --------------------------------------------------------------------


# The palette based on flat design colors: https://www.materialui.co/socialcolors
social_colors_list <- c(
  `red` = "#cd201f",
  `dark red` = "#b92b27",
  `purple` = "#ea4c89",
  `deep purple` = "#410093",
  `blue` = "#0077B5",
  `light blue` = "#55acee",
  `cyan` = "#1ab7ea",
  `teal` = "#00b489",
  `green` = "#3aaf85",
  `light green` = "#25D366",
  `yellow` = "#FFFC00",
  `amber` = "#f57d00",
  `orange` = "#ff6600",
  `deep orange` = "#ff3300",
  `grey` = "#34465d",
  `blue grey` = "#21759b"
)


#' Extract Social colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the Social color
#' palette. Use `social_colors()` to see all available colors.
#'
#' @inheritParams flat_colors
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' social_colors()
#'
#' social_colors("dark red", "teal")
#' @export
social_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(social_colors_list)
  }

  social_colors_list[cols]
}


social_palettes <- list(
  `full` = social_colors(),
  `ice` = social_colors("purple", "deep purple", "blue", "light blue"),
  `rainbow` = social_colors(
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
  `contrast` = social_colors("blue", "green", "amber", "purple", "red"),
  `light` = material_colors("light blue", "purple", "yellow", "light green", "deep orange"),
  `complement` = social_colors(
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


#' Social color palette
#'
#' The palette based [Social colors](https://materialui.co/socialcolors).
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_social()`][scale_color_social].
#'
#' @export
palette_social <- function(palette = "complement", reverse = FALSE, ...) {
  .retrieve_palette(palette, social_palettes, reverse = reverse, ...)
}
