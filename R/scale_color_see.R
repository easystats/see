#' See color palette
#'
#' The See color palette. Use `scale_color_see_d()` for *discrete*
#' categories and `scale_color_see_c()` for a *continuous* scale.
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
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_point() +
#'   theme_abyss() +
#'   scale_colour_see(palette = "light")
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_see_c(palette = "rainbow")
#' @export
scale_color_see <- function(palette = "contrast",
                            discrete = TRUE,
                            reverse = FALSE,
                            aesthetics = "color",
                            ...) {
  pal <- palette_see(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, paste0("see_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_see
#' @export
scale_color_see_d <- function(palette = "contrast",
                              discrete = TRUE,
                              reverse = FALSE,
                              aesthetics = "color",
                              ...) {
  scale_color_see(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_see
#' @export
scale_color_see_c <- function(palette = "contrast",
                              discrete = FALSE,
                              reverse = FALSE,
                              aesthetics = "color",
                              ...) {
  scale_color_see(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
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
scale_fill_see <- function(palette = "contrast",
                           discrete = TRUE,
                           reverse = FALSE,
                           aesthetics = "fill",
                           ...) {
  pal <- palette_see(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, paste0("see_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_see
#' @export
scale_fill_see_d <- function(palette = "contrast",
                             discrete = TRUE,
                             reverse = FALSE,
                             aesthetics = "fill",
                             ...) {
  scale_fill_see(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_see
#' @export
scale_fill_see_c <- function(palette = "contrast",
                             discrete = FALSE,
                             reverse = FALSE,
                             aesthetics = "fill",
                             ...) {
  scale_fill_see(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
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


#' Extract See colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the See color
#' palette. Use `see_colors()` to see all available colors.
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
  `full` = see_colors(),
  `ice` = see_colors("indigo", "blue", "blue grey", "cyan", "light blue"),
  `rainbow` = see_colors("purple", "deep purple", "indigo", "blue", "light blue", "green", "light green", "lime", "amber", "orange", "red", "pink"),
  `contrast` = see_colors("blue", "orange", "yellow", "green", "red"),
  `complement` = see_colors("blue", "blue grey", "green", "light green", "yellow", "amber", "red"),
  `light` = see_colors("light blue", "pink", "lime", "light green", "orange")
)


#' See design color palette
#'
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_see()`][scale_color_see].
#'
#' @export
palette_see <- function(palette = "contrast", reverse = FALSE, ...) {
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
    warning(insight::format_message(msg), call. = FALSE)
    palette <- 1
  }
  pal <- palette_list[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
