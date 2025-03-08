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
#'   scale_fill_uke_d()
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   theme_abyss() +
#'   scale_colour_see(palette = "light")
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_uke_c(palette = "rainbow")
#' @export
scale_color_uke <- function(palette = NULL,
                            discrete = TRUE,
                            reverse = FALSE,
                            aesthetics = "color",
                            ...) {
  if (is.null(palette)) {
    palette <- ifelse(discrete, "uke", "gradient")
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
  `blue` = "#00529A",
  `light blue` = "#E3E5F2",
  `brown` = "#9E9084",
  `light brown` = "#E9E4E1",
  `dark grey` = "#58595B",
  `light grey` = "#E6E7E8",
  `red` = "#B12833",
  `light red` = "#F4E6E1",
  `green` = "#6BC399",
  `light green` = "#DFF0E6",
  `yellow` = "#FFDF00",
  `light yellow` = "#FFF3BE",
  `orange` = "#F58021",
  `light orange` = "#FEE8D4",
  `blue grey` = "#6C90A7",
  `light blue grey` = "#E7EAEE",
  `purple` = "#B093BF",
  `light purple` = "#EFEAF3",
  `cyan` = "#5BC6CC",
  `light cyan` = "#DEF1F1"
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
#' uke_colors("blue", "cyan")
#' @export
uke_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(uke_colors_list)
  }

  uke_colors_list[cols]
}


uke_palettes <- list(
  `uke` = uke_colors("blue", "cyan", "orange", "blue grey", "red"),
  `full` = uke_colors(),
  `ice` = uke_colors("cyan", "blue grey", "light green", "light blue"),
  `contrast` = uke_colors("blue", "orange", "yellow", "green", "red"),
  `complement` = uke_colors("blue", "light blue", "green", "light green", "yellow", "light yellow", "red", "light red"),
  `light` = uke_colors("light blue", "light cyan", "light orange", "light blue grey", "light red"),
  `dark` = uke_colors("blue", "brown", "dark grey", "red", "green", "yellow", "orange", "blue grey", "purple", "cyan"),
  `gradient` = uke_colors("blue", "blue grey", "purple", "red", "orange", "yellow"),
  `light gradient` = uke_colors("light blue", "light blue grey", "light purple", "light red", "light orange", "light yellow")
)


#' UKE design color palette
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_uke()`][scale_color_uke].
#'
#' @export
palette_uke <- function(palette = "contrast", reverse = FALSE, ...) {
  .retrieve_palette(palette, see_palettes, reverse = reverse, ...)
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
