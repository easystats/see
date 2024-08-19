#' Tate Modern color palette
#'
#' A color palette inspired by paintings in the Tate Modern Art Museum in London.
#' Use `scale_color_tate_d()` for *discrete* categories and
#' `scale_color_tate_c()` for a *continuous* scale.
#'
#' @inheritParams palette_tate
#' @inheritParams scale_color_flat
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_tate_d()
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   theme_abyss() +
#'   scale_colour_tate(palette = "light")
#' @export
scale_color_tate <- function(palette = "modern",
                             discrete = TRUE,
                             reverse = FALSE,
                             aesthetics = "color",
                             ...) {
  pal <- palette_tate(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_tate
#' @export
scale_color_tate_d <- function(palette = "modern",
                               discrete = TRUE,
                               reverse = FALSE,
                               aesthetics = "color",
                               ...) {
  scale_color_tate(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_tate
#' @export
scale_color_tate_c <- function(palette = "modern",
                               discrete = FALSE,
                               reverse = FALSE,
                               aesthetics = "color",
                               ...) {
  scale_color_tate(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_tate
#' @export
scale_colour_tate <- scale_color_tate

#' @rdname scale_color_tate
#' @export
scale_colour_tate_c <- scale_color_tate_c

#' @rdname scale_color_tate
#' @export
scale_colour_tate_d <- scale_color_tate_d





# Fill --------------------------------------------------------------------



#' @rdname scale_color_tate
#' @export
scale_fill_tate <- function(palette = "modern",
                            discrete = TRUE,
                            reverse = FALSE,
                            aesthetics = "fill",
                            ...) {
  pal <- palette_tate(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_tate
#' @export
scale_fill_tate_d <- function(palette = "modern",
                              discrete = TRUE,
                              reverse = FALSE,
                              aesthetics = "fill",
                              ...) {
  scale_fill_tate(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_tate
#' @export
scale_fill_tate_c <- function(palette = "modern",
                              discrete = FALSE,
                              reverse = FALSE,
                              aesthetics = "fill",
                              ...) {
  scale_fill_tate(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}



# Palette --------------------------------------------------------------------


tate_colors_list <- c(
  # history palette
  `dark green` = "#625F0E",
  brown = "#543111",
  amber = "#B47C01",
  grey = "#595645",
  beige = "#9A8351",
  # fontana palette
  scarlett = "#7D2D36",
  red = "#A5102E",
  rose = "#C97B6F",
  yellow = "#C78F52",
  # light palettes
  `light green` = "#92901F",
  `light brown` = "#844C22",
  `light amber` = "#D49C02",
  `light grey`= "#898765",
  `light beige` = "#C4A362",
  `light scarlett` = "#A73C46",
  `light red` = "#C51A38",
  `light rose` = "#E49B90",
  `light yellow` = "#E7A963",
  # other
  mint = "#96FFFA",
  teal = "#008080",
  orange = "#FF7F00",
  blue = "#6495ED",
  lavender = "#DDA0DD"
)


#' Extract tate colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the tate color
#' palette. Use `tate_colors()` to see all available colors.
#'
#' @inheritParams flat_colors
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' tate_colors()
#'
#' tate_colors("indigo", "lime")
#' @export
tate_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(tate_colors_list)
  }

  tate_colors_list[cols]
}


tate_palettes <- list(
  full = tate_colors(),
  modern = tate_colors("yellow", "rose", "red", "brown", "grey", "dark green"),
  mix = tate_colors (
    "light rose", "light scarlett", "leight beige", "light grey",
    "blue", "orange", "teal", "mint"
  ),
  history = tate_colors("dark green", "grey", "brown", "beige", "amber"),
  fontana = tate_colors("scarlett", "red", "rose", "yellow"),
  light = tate_colors(
    "light yellow", "light rose", "light red", "light brown",
    "light grey", "light green"
  )
)


#' Tate Modern color palette
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_tate()`][scale_color_tate].
#'
#' @export
palette_tate <- function(palette = "modern", reverse = FALSE, ...) {
  .retrieve_palette(palette, tate_palettes, reverse = reverse, ...)
}





# helper -----------------------

.retrieve_palette <- function(palette, palette_list, reverse = FALSE, ...) {
  if (!palette %in% names(palette_list)) {
    msg <- c(paste0(
      "Palette name not available. `palette` must be one of ",
      datawizard::text_concatenate(names(palette_list), last = " or ", enclose = "`"),
      "."
    ), "Using default palette now.")
    insight::format_warning(msg)
    palette <- 1
  }
  pal <- palette_list[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
