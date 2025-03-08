#' UKE color palette
#'
#' The UKE color palette, based on the color scales from the University Medical
#' Center Hamburg-Eppendorf ([https://www.uke.de/]).
#'
#' @inheritParams palette_uke
#' @inheritParams scale_color_flat
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_uke()
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_uke(palette = "gradient")
#' @export
scale_color_uke <- function(palette = NULL,
                            discrete = TRUE,
                            reverse = FALSE,
                            aesthetics = "color",
                            ...) {
  if (is.null(palette)) {
    palette <- ifelse(discrete, "full", "gradient")
  }

  pal <- palette_uke(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


# Aliases -----------------------------------------------------------------


#' @rdname scale_color_uke
#' @export
scale_color_uke_d <- function(palette = NULL,
                              discrete = TRUE,
                              reverse = FALSE,
                              aesthetics = "color",
                              ...) {
  scale_color_uke(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_uke
#' @export
scale_color_uke_c <- function(palette = NULL,
                              discrete = FALSE,
                              reverse = FALSE,
                              aesthetics = "color",
                              ...) {
  scale_color_uke(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_uke
#' @export
scale_colour_uke <- scale_color_uke

#' @rdname scale_color_uke
#' @export
scale_colour_uke_c <- scale_color_uke_c

#' @rdname scale_color_uke
#' @export
scale_colour_uke_d <- scale_color_uke_d


# Fill --------------------------------------------------------------------


#' @rdname scale_color_uke
#' @export
scale_fill_uke <- function(palette = NULL,
                           discrete = TRUE,
                           reverse = FALSE,
                           aesthetics = "fill",
                           ...) {
  if (is.null(palette)) {
    palette <- ifelse(discrete, "uke", "gradient")
  }
  pal <- palette_uke(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_uke
#' @export
scale_fill_uke_d <- function(palette = NULL,
                             discrete = TRUE,
                             reverse = FALSE,
                             aesthetics = "fill",
                             ...) {
  scale_fill_uke(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_uke
#' @export
scale_fill_uke_c <- function(palette = NULL,
                             discrete = FALSE,
                             reverse = FALSE,
                             aesthetics = "fill",
                             ...) {
  scale_fill_uke(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}


# Palette --------------------------------------------------------------------


uke_colors_list <- c(
  blue = "#004992",
  brown = "#AA9C8F",
  cyan = "#68C3CD",
  green = "#8ABD24",
  purple = "#BA9BC5",
  yellow = "#FFDF00",
  orange = "#FCBE0E",
  amber = "#EF7B05",
  livid = "#7296AF",
  mint = "#74C095",
  grey = "#575756",
  red = "#B22229"
)


#' Extract UKE colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the UKE color
#' palette. Use `uke_colors()` to see all available colors.
#'
#' @inheritParams flat_colors
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' uke_colors()
#'
#' uke_colors("blue", "mint")
#' @export
uke_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(uke_colors_list)
  }

  uke_colors_list[cols]
}


uke_palettes <- list(
  `uke` = uke_colors("blue", "cyan", "orange", "livid", "red"),
  `full` = uke_colors(),
  `ice` = uke_colors("cyan", "livid", "mint", "blue", "grey"),
  `contrast` = uke_colors("blue", "green", "yellow", "red", "purple"),
  `gradient` = uke_colors("yellow", "orange", "amber", "purple", "cyan", "livid", "blue")
)


#' UKE design color palette
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_uke()`][scale_color_uke].
#'
#' @export
palette_uke <- function(palette = "uke", reverse = FALSE, ...) {
  .retrieve_palette(palette, uke_palettes, reverse = reverse, ...)
}
