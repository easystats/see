#' Okabe-Ito color palette
#'
#' The Okabe-Ito color palette was proposed by Okabe and Ito (2008) as a
#' qualitative color palette that is accessible to people with a variety of
#' forms of color vision deficiency. In addition to being accessible, it
#' includes 9 vivid colors that are readily nameable and include colors that
#' correspond to major primary and secondary colors (e.g., red, yellow, blue).
#'
#' The Okabe-Ito palette is included in the base R [palette.colors()].
#' These functions make this palette easier to use with *ggplot2*.
#'
#' The Okabe-Ito palette is only available as a discrete palette.
#' For color-accessible continuous variables, consider
#' [the viridis palettes][ggplot2::scale_colour_viridis_d()].
#'
#' @inheritParams palette_okabeito
#' @inheritParams scale_color_flat
#'
#' @references
#' Okabe, M., & Ito, K. (2008). Color universal design (CUD):
#' How to make figures and presentations that are friendly to colorblind people.
#' https://jfly.uni-koeln.de/color/#pallet (Original work published 2002)
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_okabeito()
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_oi(palette = "black_first")
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_oi(order = c(1, 5, 6, 2, 4, 3, 7))
#' @export
scale_color_okabeito <- function(palette = "full", reverse = FALSE, order = 1:9, aesthetics = "color", ...) {
  discrete_scale(
    aesthetics = aesthetics,
    paste0("okabeito_", palette),
    palette = palette_okabeito(palette = palette, reverse = reverse, order = order),
    ...
  )
}

# Fill --------------------------------------------------------------------

#' @rdname scale_color_okabeito
#' @export
scale_fill_okabeito <- function(palette = "full", reverse = FALSE, order = 1:9, aesthetics = "fill", ...) {
  discrete_scale(
    aesthetics = aesthetics,
    paste0("okabeito_", palette),
    palette = palette_okabeito(palette = palette, reverse = reverse, order = order),
    ...
  )
}

# Aliases -----------------------------------------------------------------

#' @rdname scale_color_okabeito
#' @export
scale_colour_okabeito <-
  scale_colour_oi <-
  scale_color_oi <-
  scale_color_okabeito

#' @rdname scale_color_okabeito
#' @export
scale_fill_oi <- scale_fill_okabeito



# Palette --------------------------------------------------------------------

# The palette from: https://jfly.uni-koeln.de/color/#pallet
okabeito_colors_list <- c(
  `orange` = "#E69F00",
  `light blue` = "#56B4E9",
  `green` = "#009E73",
  `yellow` = "#F0E442",
  `blue` = "#0072B2",
  `red` = "#D55E00",
  `purple` = "#CC79A7",
  `grey` = "#999999",
  `black` = "#000000",
  `sky blue` = "#56B4E9",
  `bluish green` = "#009E73",
  `vermillion` = "#D55E00",
  `reddish purple` = "#CC79A7"
)


#' Extract Okabe-Ito colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the Okabe-Ito palette.
#' Use `okabeito_colors()` to see all available colors.
#'
#' @inheritParams flat_colors
#' @param original_names Logical. Should the colors be named using the original
#'   names used by Okabe and Ito (2008), such as "vermillion" (`TRUE`), or
#'   simplified names, such as "red" (`FALSE`, default)?
#'   Only used if no colors are specified (to see all available colors).
#' @param black_first Logical. Should black be first (`TRUE`) or last (`FALSE`, default)
#'   in the color palette? Only used if no colors are specified (to see all
#'   available colors).
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' okabeito_colors()
#'
#' okabeito_colors(c("red", "light blue", "orange"))
#'
#' okabeito_colors(original_names = TRUE)
#'
#' okabeito_colors(black_first = TRUE)
#' @export
okabeito_colors <- function(..., original_names = FALSE, black_first = FALSE) {
  cols <- c(...)

  if (is.null(cols)) {
    if (isTRUE(original_names)) {
      cols <- c("orange", "light blue", "green", "yellow", "blue", "red", "purple", "grey", "black")
    } else {
      cols <- c("orange", "sky blue", "bluish green", "yellow", "blue", "vermillion", "reddish purple", "grey", "black")
    }
    if (isTRUE(black_first)) cols <- union("black", cols)
  }

  okabeito_colors_list[cols]
}

#' @rdname okabeito_colors
#' @export
oi_colors <- okabeito_colors

okabeito_palettes <- list(
  `full` = okabeito_colors(),
  `black_first` = okabeito_colors(black_first = TRUE)
)


#' Okabe-Ito color palette
#'
#' The palette based proposed by Okabe and Ito (2008).
#'
#' @inheritParams palette_flat
#' @param order A vector of numbers from 1 to 9 indicating the order of colors to use
#'   (default: `1:9`)
#'
#' @references
#' Okabe, M., & Ito, K. (2008). Color universal design (CUD):
#' How to make figures and presentations that are friendly to colorblind people.
#' https://jfly.uni-koeln.de/color/#pallet (Original work published 2002)
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_material()`][scale_color_material].
#'
#' @export
palette_okabeito <- function(palette = "full", reverse = FALSE, order = 1:9, ...) {
  if (!palette %in% names(okabeito_palettes)) {
    msg <- c(paste0(
      "Palette name not available. `palette` must be one of ",
      datawizard::text_concatenate(names(okabeito_palettes), last = " or ", enclose = "`"),
      "."
    ), "Using default palette now.")
    warning(insight::format_message(msg), call. = FALSE)
    palette <- "full"
  }

  pal <- okabeito_palettes[[palette]]

  stopifnot(
    "`order` must be a vector of integers." = is.numeric(order),
    "All elements of `order` must be greater than 0 and less than 10." = all(order > 0 & order <= 9)
  )
  pal <- pal[order]

  if (reverse) pal <- rev(pal)

  function(n) {
    if (n > length(pal)) {
      warning(insight::format_message(
        "The number of colors requested `n` is too large.",
        paste0("The maximum number of colors is ", length(pal), "."),
        paste0("Returning a palette with ", length(pal), " colors.")
      ))
      n <- length(pal)
    }
    unname(pal[seq_len(n)])
  }
}

#' @rdname palette_okabeito
#' @export
palette_oi <- palette_okabeito
