#' Color palettes from color-hex
#'
#' @description
#'
#' This function creates color scales based on palettes from <https://www.color-hex.com/>.
#' This website provides a large number of user-submitted color palettes.
#' This function downloads a requested color palette from <https://www.color-hex.com/>.
#' and creates a `{ggplot2}` color scale from the provided hex codes.
#'
#' Use `scale_color_colorhex_d` for *discrete* categories and
#' `scale_color_colorhex_c` for a *continuous* scale.
#'
#' @inheritParams palette_colorhex
#' @inheritParams scale_color_flat
#'
#' @note
#' The default [Josiah color palette (number 1014416)](https://www.color-hex.com/color-palette/1014416)
#' is available without an internet connection. All other color palettes require
#' an internet connection to download and access.
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, color = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_color_colorhex_d(palette = 1014416)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_violin() +
#'   theme_modern() +
#'   scale_fill_colorhex_d(palette = 1014416)
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_colorhex_c(palette = 1014416)
#' @export
scale_color_colorhex <- function(
  palette = 1014416,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "color",
  ...
) {
  pal <- palette_colorhex(palette = palette, reverse = reverse)
  pal_name <- attr(pal, "name")
  if (is.null(pal_name)) {
    pal_name <- palette
  }

  if (discrete) {
    discrete_scale(
      aesthetics = aesthetics,
      palette = pal,
      ...
    )
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


# Aliases -----------------------------------------------------------------

#' @rdname scale_color_colorhex
#' @export
scale_color_colorhex_d <- function(
  palette = 1014416,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "color",
  ...
) {
  scale_color_colorhex(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_colorhex
#' @export
scale_color_colorhex_c <- function(
  palette = 1014416,
  discrete = FALSE,
  reverse = FALSE,
  aesthetics = "color",
  ...
) {
  scale_color_colorhex(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_colorhex
#' @export
scale_colour_colorhex <- scale_color_colorhex

#' @rdname scale_color_colorhex
#' @export
scale_colour_colorhex_c <- scale_color_colorhex_c

#' @rdname scale_color_colorhex
#' @export
scale_colour_colorhex_d <- scale_color_colorhex_d


# Fill --------------------------------------------------------------------

#' @rdname scale_color_colorhex
#' @export
scale_fill_colorhex <- function(
  palette = 1014416,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "fill",
  ...
) {
  pal <- palette_colorhex(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_colorhex
#' @export
scale_fill_colorhex_d <- function(
  palette = 1014416,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "fill",
  ...
) {
  scale_fill_colorhex(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_colorhex
#' @export
scale_fill_colorhex_c <- function(
  palette = 1014416,
  discrete = FALSE,
  reverse = FALSE,
  aesthetics = "fill",
  ...
) {
  scale_fill_colorhex(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}


# Palette --------------------------------------------------------------------

#' Color palettes from <https://www.color-hex.com/>
#'
#' This function downloads a requested color palette from <https://www.color-hex.com/>.
#' This website provides a large number of user-submitted color palettes.
#'
#' @note
#' The default [Josiah color palette (number 1014416)](https://www.color-hex.com/color-palette/1014416)
#' is available without an internet connection. All other color palettes require
#' an internet connection to download and access.
#'
#' @param palette The numeric code for a palette at <https://www.color-hex.com/>.
#'   For example, `1014416` for the
#'   [Josiah color palette (number 1014416)](https://www.color-hex.com/color-palette/1014416).
#' @inheritParams palette_flat
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_colorhex()`][scale_color_colorhex].
#'
#' @export
palette_colorhex <- function(palette = 1014416, reverse = FALSE, ...) {
  if (!is.numeric(palette) && suppressWarnings(is.na(as.numeric(palette)))) {
    insight::format_error(
      "`palette` must be the numeric code for a color palette at <https://www.color-hex.com/>"
    )
  }

  if (palette == 1014416) {
    pal <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")
    pal_name <- "Josiah"
  } else {
    insight::check_if_installed(
      "curl",
      reason = "to retrieve palettes from <https://www.color-hex.com/>"
    )

    curl_url <- paste0("https://www.color-hex.com//color-palette/", palette)
    con <- curl::curl(url = curl_url)
    curl_res <- tryCatch(
      suppressWarnings(readLines(con)),
      error = function(e) e
    )
    close(con)

    if (inherits(curl_res, "error")) {
      insight::format_error(
        "Could not reach <color-hex.com/>. Check your internet connection."
      )
    }

    curl_res <- grep("description", curl_res, fixed = TRUE, value = TRUE)
    if (!length(curl_res)) {
      insight::format_error(paste0(
        "Requested palette '",
        palette,
        "' not found. Check the palette ID."
      ))
    }

    pal <- unlist(
      regmatches(curl_res, gregexpr("#[a-fA-F0-9]{6}", curl_res))[[1]],
      use.names = FALSE
    )
    pal_name <- gsub("(.*)content=\"(.*) color palette(.*)", "\\2", curl_res)
  }

  if (reverse) {
    pal <- rev(pal)
  }

  pal <- grDevices::colorRampPalette(pal, ...)
  attr(pal, "name") <- pal_name
  return(pal)
}
