#' Paul Tol color palettes
#'
#' Tol (2021) presents a series of palettes built with mathematical principles that
#' are appropriate for diverse types of data. The colors in these schemes are:
#' - Visually distinct for all people, including viewers with color vision deficiencies
#' - Distinct from black and white
#' - Distinct on screen and paper,
#' - Cohesive; that is, they match well together
#'
#' Tol provides palettes appropriate to the 3 main types of data:
#' 1. Qualitative data – nominal or categorical data, where magnitude differences are not relevant.
#' 2. Diverging data – data ordered between two extremes where the midpoint is important.
#' 3. Sequential data – data ordered from low to high.
#'
#' Available palettes for each type of data are:
#' - Qualitative: bright, high-contrast, vibrant, muted, medium-contrast, pale, dark, light, ground_cover
#' - Diverging: sunset, BuRd, PRGn
#' - Sequential: YlOrBr, iridescent, rainbow_discrete, rainbow_smooth
#'
#' <!-- For rainbow_discrete, pick the optimal set based on the number of colors,
#'      For rainbow_smooth, include a range argument with examples to start/end at off-white vs. purple, red vs brown
#'      -->
#'
#' <!-- put a table with recommended uses here -->
#'
#' ## Colors for missing or invalid data
#'
#' A useful feature of Tol's diverging and sequential palettes is that he
#' provides a recommended color to use for data that fall outside the data
#' range represented by the color scale (e.g., for invalid or missing data).
#' These colors are chosen to be highly distinct from the main color palette.
#'
#' @inheritParams palette_tol
#' @inheritParams scale_color_flat
#'
#' @references
#' Tol, P. (2021). Colour schemes (SRON/EPS Technical Note No. 09-002; Version 3.2).
#' SRON. https://personal.sron.nl/~pault/data/colourschemes.pdf (Original work published 2009)
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
#' # for the original brighter yellow color suggested by Okabe and Ito
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_oi(palette = "full")
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_oi(order = c(1, 5, 6, 2, 4, 3, 7))
#' @export
scale_color_okabeito <- function(palette = "full", reverse = FALSE, order = 1:9, aesthetics = "color", ...) {
  discrete_scale(
    aesthetics = aesthetics,
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
    palette = palette_okabeito(palette = palette, reverse = reverse, order = order),
    ...
  )
}

# Aliases -----------------------------------------------------------------

#' @rdname scale_color_okabeito
#' @export
scale_colour_okabeito <- scale_color_okabeito

#' @rdname scale_color_okabeito
#' @export
scale_colour_oi <- scale_color_okabeito

#' @rdname scale_color_okabeito
#' @export
scale_color_oi <- scale_color_okabeito

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
  `reddish purple` = "#CC79A7",
  `dark yellow` = "#F5C710",
  `amber` = "#F5C710"
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
#' @param amber If amber color should replace yellow in the palette.
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
okabeito_colors <- function(..., original_names = FALSE, black_first = FALSE, amber = TRUE) {
  cols <- c(...)

  if (!is.null(cols)) {
    return(okabeito_colors_list[cols])
  }

  yellow_col <- if (isTRUE(amber)) "amber" else "yellow"

  if (isTRUE(original_names)) {
    cols <- c("orange", "sky blue", "bluish green", yellow_col, "blue", "vermillion", "reddish purple", "grey", "black")
  } else {
    cols <- c("orange", "light blue", "green", yellow_col, "blue", "red", "purple", "grey", "black")
  }

  if (isTRUE(black_first)) cols <- union("black", cols)

  okabeito_colors_list[cols]
}

#' @rdname okabeito_colors
#' @export
oi_colors <- okabeito_colors

okabeito_palettes <- list(
  `full` = okabeito_colors(black_first = FALSE, amber = TRUE),
  `black_first` = okabeito_colors(black_first = TRUE, amber = TRUE),
  `full_original` = okabeito_colors(black_first = FALSE, amber = FALSE),
  `black_original` = okabeito_colors(black_first = TRUE, amber = FALSE)
)


#' Paul Tol's color palettes
#'
#' The palettes based proposed by Okabe and Ito (2008).
#'
#' @inheritParams palette_flat
#' @param order A vector of numbers from 1 to 9 indicating the order of colors to use
#'   (default: `1:9`)
#'
#' @references
#' Tol, P. (2021). Colour schemes (SRON/EPS Technical Note No. 09-002; Version 3.2).
#' SRON. https://personal.sron.nl/~pault/data/colourschemes.pdf (Original work published 2009)
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_tol()`][scale_color_tol].
#'
#' @export
palette_tol <- function(palette = "full_amber", reverse = FALSE, order = 1:9, ...) {
  if (!palette %in% names(tol_palettes)) {
    msg <- c(paste0(
      "Palette name not available. `palette` must be one of ",
      datawizard::text_concatenate(names(okabeito_palettes), last = " or ", enclose = "`"),
      "."
    ), "Using default palette now.")
    insight::format_warning(msg)
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
      insight::format_warning(
        "The number of colors requested `n` is too large.",
        paste0("The maximum number of colors is ", length(pal), "."),
        paste0("Returning a palette with ", length(pal), " colors.")
      )
      n <- length(pal)
    }
    unname(pal[seq_len(n)])
  }
}

#' @rdname palette_okabeito
#' @export
palette_oi <- palette_okabeito
