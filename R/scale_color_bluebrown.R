#' Blue-brown color palette
#'
#' A blue-brown color palette. Use `scale_color_bluebrown_d()` for
#' *discrete* categories and `scale_color_bluebrown_c()` for
#' a *continuous* scale.
#'
#' @inheritParams palette_bluebrown
#' @inheritParams scale_color_flat
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_bluebrown_d()
#' @export
scale_color_bluebrown <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, aesthetics = "color", ...) {
  pal <- palette_bluebrown(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_bluebrown
#' @export
scale_color_bluebrown_d <- function(palette = "contrast",
                                    discrete = TRUE,
                                    reverse = FALSE,
                                    aesthetics = "color",
                                    ...) {
  scale_color_bluebrown(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_bluebrown
#' @export
scale_color_bluebrown_c <- function(palette = "contrast",
                                    discrete = FALSE,
                                    reverse = FALSE,
                                    aesthetics = "color",
                                    ...) {
  scale_color_bluebrown(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_bluebrown
#' @export
scale_colour_bluebrown <- scale_color_bluebrown

#' @rdname scale_color_bluebrown
#' @export
scale_colour_bluebrown_c <- scale_color_bluebrown_c

#' @rdname scale_color_bluebrown
#' @export
scale_colour_bluebrown_d <- scale_color_bluebrown_d





# Fill --------------------------------------------------------------------



#' @rdname scale_color_bluebrown
#' @export
scale_fill_bluebrown <- function(palette = "contrast",
                                 discrete = TRUE,
                                 reverse = FALSE,
                                 aesthetics = "fill",
                                 ...) {
  pal <- palette_bluebrown(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_bluebrown
#' @export
scale_fill_bluebrown_d <- function(palette = "contrast",
                                   discrete = TRUE,
                                   reverse = FALSE,
                                   aesthetics = "fill",
                                   ...) {
  scale_fill_bluebrown(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_bluebrown
#' @export
scale_fill_bluebrown_c <- function(palette = "contrast",
                                   discrete = FALSE,
                                   reverse = FALSE,
                                   aesthetics = "fill",
                                   ...) {
  scale_fill_bluebrown(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}





# Palette --------------------------------------------------------------------


bluebrown_colors_list <- c(
  `lightblue` = "#6DC0E0",
  `blue` = "#5B93AE",
  `darkblue` = "#1F4454",
  `grey` = "#dbdbdb",
  `lightbrown` = "#92673C",
  `brown` = "#61381A",
  `darkbrown` = "#391D07"
)


#' Extract blue-brown colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the blue-brown color palette.
#' Use `bluebrown_colors()` to see all available colors.
#'
#' @inheritParams flat_colors
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' bluebrown_colors()
#'
#' bluebrown_colors("blue", "brown")
#' @export
bluebrown_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(bluebrown_colors_list)
  }

  bluebrown_colors_list[cols]
}




bluebrown_palettes <- list(
  `full` = bluebrown_colors(),
  `contrast` = bluebrown_colors("lightblue", "blue", "darkblue", "grey", "darkbrown", "brown", "lightbrown"),
  `rainbow` = bluebrown_colors("darkblue", "blue", "lightblue", "grey", "lightbrown", "brown", "darkbrown")
)






#' Blue-brown design color palette
#'
#' The palette based on blue-brown colors.
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_bluebrown()`][scale_color_bluebrown].
#'
#' @export
palette_bluebrown <- function(palette = "contrast", reverse = FALSE, ...) {
  .retrieve_palette(palette, bluebrown_palettes, reverse = reverse, ...)
}
