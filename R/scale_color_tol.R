#' Paul Tol discrete/qualitative color palettes
#'
#' Tol (2021) presents a series of palettes built with mathematical principles
#' that are appropriate for diverse types of data. The colors in these schemes
#' are:
#' - Visually distinct for all people, including viewers with color vision
#'   deficiencies
#' - Distinct from black and white
#' - Distinct on screen and paper,
#' - Cohesive; that is, they match well together
#'
#' Tol provides palettes appropriate to the 3 main types of data:
#' 1. Qualitative data – nominal or categorical data, where magnitude
#'    differences are not relevant.
#' 2. Diverging data – data ordered between two extremes where the midpoint is
#'    important.
#' 3. Sequential data – data ordered from low to high.
#'
#' This function provides the qualitative palettes, as well as discrete rainbow
#' sequential palettes. Available palettes for each type of data are:
#' - Qualitative: `bright`, `high-contrast`, `vibrant`, `muted`,
#'   `medium-contrast`, `pale`, `dark`, `light`, `ground_cover`
#' - Diverging: `sunset`, `BuRd`, `PRGn`
#' - Sequential: `YlOrBr`, `iridescent`, `rainbow_discrete`, `rainbow_smooth`
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
#' provides a recommended color to use for data that fall outside the data range
#' represented by the color scale (e.g., for invalid or missing data). These
#' colors are chosen to be highly distinct from the main color palette.
#'
#' @inheritParams palette_tol_discrete
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
scale_color_tol_discrete <- function(palette = "bright", reverse = FALSE, order = NULL, aesthetics = "color", ...) {
  discrete_scale(
    aesthetics = aesthetics,
    palette = palette_tol_discrete(palette = palette, reverse = reverse, order = order),
    ...
  )
}

# Fill --------------------------------------------------------------------

#' @rdname scale_color_tol_discrete
#' @export
scale_fill_tol_discrete <- function(palette = "bright", reverse = FALSE, order = NULL, aesthetics = "fill", ...) {
  discrete_scale(
    aesthetics = aesthetics,
    palette = palette_tol_discrete(palette = palette, reverse = reverse, order = order),
    ...
  )
}

# Aliases -----------------------------------------------------------------

#' @rdname scale_color_tol_discrete
#' @export
scale_colour_tol_discrete <- scale_color_tol_discrete


# Palette --------------------------------------------------------------------

tol_colors_discrete_list <- list(
  bright = c(blue = "#4477AA", red = "#EE6677", green = "#228833", yellow = "#CCBB44", cyan = "#66CCEE", purple = "#AA3377", grey = "#BBBBBB"),
  `high-contrast` = c(blue = "#004488", yellow = "#DDAA33", red = "#BB5566", black = "#000000", white = "#FFFFFF"),
  vibrant = c(orange = "#EE7733", blue = "#0077BB", cyan = "#33BBEE", magenta = "#EE3377", red = "#CC3311", teal = "#009988", grey = "#BBBBBB"),
  muted = c(rose = "#CC6677", indigo = "#332288", sand = "#DDCC77", green = "#117733", cyan = "#88CCEE", wine = "#882255", teal = "#44AA99", olive = "#999933", purple = "#AA4499", grey = "#DDDDDD"),
  `medium-contrast` = c("light blue" = "#6699CC", "dark blue" = "#004488", "light yellow" = "#EECC66", "dark red" = "#994455", "dark yellow" = "#997700", "light red" = "#EE99AA", black = "#000000", white = "#FFFFFF"),
  pale = c(blue = "#BBCCEE", cyan = "#CCEEFF", green = "#CCDDAA", yellow = "#EEEEBB", red = "#FFCCCC", grey = "#DDDDDD"),
  dark = c(blue = "#222255", cyan = "#225555", green = "#225522", yellow = "#666633", red = "#663333", grey = "#555555"),
  light = c(blue = "#77AADD", orange = "#EE8866", yellow = "#EEDD88", pink = "#FFAABB", cyan = "#99DDFF", mint = "#44BB99", pear = "#BBCC33", olive = "#AAAA00", grey = "#DDDDDD"),
  # TODO: Finish rainbow color schemes
  rainbow14 = c("3", "6", "9", "10", "12", "14", "15", "16", "17", "18", "20", "22", "24", "26" = "#DC050C", "grey"),
  rainbow23 = c("1", "2", "4", "5", "7", "8", "9", "10", "11", "13", "14", "15", "16", "17", "18", "19", "21", "23", "25", "26" = "#DC050C", "27" = "#A5170E", "28" = "#72190E", "29" = "#42150A", grey = "#777777"),
  ground_cover = c(
    water = "#5566AA", "evergreen needleleaf forest" = "#117733", "deciduous needleleaf forest" = "#44AA66",
    "mixed forest" = "#55AA22", "evergreen broadleaf forest" = "#668822", "deciduous broadleaf forest" = "#88BB55",
    woodland = "#558877", "wooded grassland" = "#88BBAA", grassland = "#AADDCC", cropland = "#44AA88",
    "closed shrubland" = "#DDCC66", "open shrubland" = "#FFDD44", "bare ground" = "#FFEE88", "urband and built up" = "#BB0011"
  )
)

tol_colors_smooth_list <- list(
  # Diverging
  sunset = "#",
  BuRd = NULL,
  PRGn = NULL,
  # Sequential
  YlOrBr = NULL,
  iridescent = NULL,
  rainbow = NULL
)


#' Extract Paul Tol colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the Paul Tol palettes.
#' Use `tol_colors()` and specify `palette` to see all available colors.
#' Note that for sequential palettes, only original (non-interpolated) colors are shown.
#'
#' @param ... Character names of colors.
#' @param palette Character name of palette. Can be:
#' - Qualitative: `"bright"`, `"high-contrast"`, `"vibrant"`, `"muted"`,`
#'   "medium-contrast"`, `"pale"`, `"dark"`, `"light"`, `"ground_cover"`
#' - Diverging: `"sunset"`, `"BuRd"`, `"PRGn"`
#' - Sequential: `"YlOrBr"`, `"iridescent"`, `"rainbow_discrete"`,
#'   `"rainbow_smooth"`
#'
#' @return A character vector with color-codes.
#'
#' @examples
#' tol_colors()
#'
#' tol_colors(c("red", "light blue", "yellow"))
#'
#' tol_colors(palette = "muted")
#'
#' tol_colors(c("red", "light blue", "yellow"), palette = "muted")
#' @export
tol_colors <- function(..., palette = "bright") {
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


#' Paul Tol's color palettes
#'
#' The palettes proposed by Tol (2021).
#'
#' @param palette Character name of palette. Can be:
#' - Qualitative: `"bright"`, `"high-contrast"`, `"vibrant"`, `"muted"`,
#'   `"medium-contrast"`, `"pale"`, `"dark"`, `"light"`, `"ground_cover"`
#' - Diverging: `"sunset"`, `"BuRd"`, `"PRGn"`
#' - Sequential: `"YlOrBr"`, `"iridescent"`, `"rainbow_discrete"`,
#' - `"rainbow_smooth"`
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param order A vector of numbers indicating the order of colors to use
#' (default: `NULL` indicating to use all available colors in order).
#' @param ... For sequential palettes other than `rainbow_discrete`, additional
#' arguments to pass to [`grDevices::colorRampPalette()`].
#'
#' @references
#' Tol, P. (2021). Colour schemes (SRON/EPS Technical Note No. 09-002; Version 3.2).
#' SRON. https://personal.sron.nl/~pault/data/colourschemes.pdf (Original work published 2009)
#'
#' @details This function is usually not called directly, but from within
#' [`scale_color_tol()`].
#'
#' @export
palette_tol_discrete <- function(palette = "bright", reverse = FALSE, order = NULL, ...) {
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
    "All elements of `order` must be greater than 0 and less than 10." = order > 0 & order <= 9
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
