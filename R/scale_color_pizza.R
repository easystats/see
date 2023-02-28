# Scales -----------------------------------------------------------------

#' Pizza color palette
#'
#' The palette based on authentic neapolitan pizzas.
#' Use `scale_color_pizza_d()` for *discrete* categories and
#' `scale_color_pizza_c()` for a *continuous* scale.
#'
#' @inheritParams palette_pizza
#' @inheritParams scale_color_flat
#'
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   theme_modern() +
#'   scale_fill_pizza_d()
#'
#' ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Sepal.Length)) +
#'   geom_point() +
#'   theme_modern() +
#'   scale_color_pizza_c()
#' @export
scale_color_pizza <- function(palette = "margherita",
                              discrete = TRUE,
                              reverse = FALSE,
                              aesthetics = "color",
                              ...) {
  pal <- palette_pizza(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, paste0("pizza_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_pizza
#' @export
scale_color_pizza_d <- function(palette = "margherita",
                                discrete = TRUE,
                                reverse = FALSE,
                                aesthetics = "color",
                                ...) {
  scale_color_pizza(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_pizza
#' @export
scale_color_pizza_c <- function(palette = "margherita",
                                discrete = FALSE,
                                reverse = FALSE,
                                aesthetics = "color",
                                ...) {
  scale_color_pizza(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_pizza
#' @export
scale_colour_pizza <- scale_color_pizza

#' @rdname scale_color_pizza
#' @export
scale_colour_pizza_c <- scale_color_pizza_c

#' @rdname scale_color_pizza
#' @export
scale_colour_pizza_d <- scale_color_pizza_d



# Fill --------------------------------------------------------------------

#' @rdname scale_color_pizza
#' @export
scale_fill_pizza <- function(palette = "margherita",
                             discrete = TRUE,
                             reverse = FALSE,
                             aesthetics = "fill",
                             ...) {
  pal <- palette_pizza(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale(aesthetics = aesthetics, paste0("pizza_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), aesthetics = aesthetics, ...)
  }
}


#' @rdname scale_color_pizza
#' @export
scale_fill_pizza_d <- function(palette = "margherita",
                               discrete = TRUE,
                               reverse = FALSE,
                               aesthetics = "fill",
                               ...) {
  scale_fill_pizza(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}

#' @rdname scale_color_pizza
#' @export
scale_fill_pizza_c <- function(palette = "margherita",
                               discrete = FALSE,
                               reverse = FALSE,
                               aesthetics = "fill",
                               ...) {
  scale_fill_pizza(
    palette = palette,
    discrete = discrete,
    reverse = reverse,
    aesthetics = aesthetics,
    ...
  )
}




# Palette --------------------------------------------------------------------



# The palette based on this image:
# https://www.scattidigusto.it/wp-content/uploads/2018/03/pizza-margherita-originale-Scatti-di-Gusto.jpg
pizza_colors_list <- c(
  `tomato` = "#CE3722",
  `mozzarella` = "#EBE8E1",
  `basil` = "#768947",
  `crust` = "#E7CBA3",
  `coal` = "#302124",
  `diavola` = "#642118"
)


#' Extract pizza colors as hex codes
#'
#' @param ... Character names of pizza ingredients.
#'
#' @return A character vector with color-codes.
#'
#' @export
pizza_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(pizza_colors_list)
  }

  pizza_colors_list[cols]
}




pizza_palettes <- list(
  `margherita` = pizza_colors("tomato", "mozzarella", "basil"),
  `margherita_crust` = pizza_colors("crust", "tomato", "mozzarella", "basil", "coal"),
  `diavola` = pizza_colors("tomato", "mozzarella", "basil", "diavola"),
  `diavola_crust` = pizza_colors("crust", "tomato", "mozzarella", "basil", "diavola", "coal")
)



#' Pizza color palette
#'
#' The palette based on authentic neapolitan pizzas.
#'
#' @param palette Pizza type. Can be "margherita" (default), "margherita_crust",
#'  "diavola" or "diavola_crust".
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments to pass to [`colorRampPalette()`][colorRampPalette].
#'
#' @details This function is usually not called directly, but from within
#'   [`scale_color_pizza()`][scale_color_pizza].
#'
#' @export
palette_pizza <- function(palette = "margherita", reverse = FALSE, ...) {
  .retrieve_palette(palette, pizza_palettes, reverse = reverse, ...)
}
