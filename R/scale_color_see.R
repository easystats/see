#' see color palette
#'
#' The see color palette. Use \code{scale_color_see_d()} for \emph{discrete}
#' categories and \code{scale_color_see_c()} for a \emph{continuous} scale.
#'
#' @inheritParams palette_see
#' @inheritParams scale_color_flat
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_see_d()
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_see_d(palette = "ice")
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_see_c(palette = "rainbow")
#' @export
scale_color_see <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  pal <- palette_see(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("see_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_see
#' @export
scale_color_see_d <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  scale_color_see(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_see
#' @export
scale_color_see_c <- function(palette = "contrast", discrete = FALSE, reverse = FALSE, ...) {
  scale_color_see(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_see
#' @export
scale_colour_see <- scale_color_see

#' @rdname scale_color_see
#' @export
scale_colour_see_c <- scale_color_see_c

#' @rdname scale_color_see
#' @export
scale_colour_see_d <- scale_color_see_d





# Fill --------------------------------------------------------------------



#' @rdname scale_color_see
#' @export
scale_fill_see <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  pal <- palette_see(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("see_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' @rdname scale_color_see
#' @export
scale_fill_see_d <- function(palette = "contrast", discrete = TRUE, reverse = FALSE, ...) {
  scale_fill_see(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_see
#' @export
scale_fill_see_c <- function(palette = "contrast", discrete = FALSE, reverse = FALSE, ...) {
  scale_fill_see(palette = palette, discrete = discrete, reverse = reverse, ...)
}





# Palette --------------------------------------------------------------------




see_colors_list <- c(
  `red` = "#d32626",
  `pink` = "#b5076b",
  `purple` = "#5c2a9d",
  `deep purple` = "#45046a",
  `indigo` = "#303960",
  `blue` = "#1b6ca8",
  `light blue` = "#03A9F4",
  `cyan` = "#0a97b0",
  `green` = "#438a5e",
  `light green` = "#bac964",
  `lime` = "#f7fbe1",
  `yellow` = "#fbd46d",
  `amber` = "#ff9c71",
  `orange` = "#fb7813",
  `grey` = "#e7dfd5",
  `blue grey` = "#3b6978"
)


#' Extract see colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the see color palette. Use \code{see_colors()} to see all available color.
#'
#' @inheritParams flat_colors
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' see_colors()
#'
#' see_colors("indigo", "lime")
#' @export
see_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(see_colors_list)
  }

  see_colors_list[cols]
}




see_palettes <- list(
  `full`  = see_colors(),
  `ice`  = see_colors("purple", "deep purple", "indigo", "blue", "light blue"),
  `rainbow` = see_colors("purple", "deep purple", "indigo", "blue", "light blue", "green", "light green", "lime", "amber", "orange", "red", "pink"),
  `contrast` = see_colors("blue", "green", "amber", "indigo", "red"),
  `complement` = see_colors("blue", "blue grey", "green", "light green","yellow", "amber", "red")
)






#' see design color palette
#'
#' see design color palette.
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   \code{\link[=scale_color_see]{scale_color_see()}}.
#'
#' @importFrom grDevices colorRampPalette
#' @export
palette_see <- function(palette = "contrast", reverse = FALSE, ...) {
  pal <- see_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


