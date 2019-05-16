# Scales -----------------------------------------------------------------



#' Pizza color palette
#'
#' The palette based on authentic neapolitan pizzas.
#' Use \code{scale_color_pizza_d} for \emph{discrete} categories and
#' \code{scale_color_pizza_c} for a \emph{continuous} scale.
#'
#' @inheritParams palette_pizza
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE.
#'
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
#'
#' @export
scale_color_pizza <- function(palette = "margherita", discrete = TRUE, reverse = FALSE, ...) {
  pal <- palette_pizza(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("pizza_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}



# Aliases -----------------------------------------------------------------


#' @rdname scale_color_pizza
#' @export
scale_color_pizza_d <- function(palette = "margherita", discrete = TRUE, reverse = FALSE, ...) {
  scale_color_pizza(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_pizza
#' @export
scale_color_pizza_c <- function(palette = "margherita", discrete = FALSE, reverse = FALSE, ...) {
  scale_color_pizza(palette = palette, discrete = discrete, reverse = reverse, ...)
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
scale_fill_pizza <- function(palette = "margherita", discrete = TRUE, reverse = FALSE, ...) {
  pal <- palette_pizza(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("pizza_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' @rdname scale_color_pizza
#' @export
scale_fill_pizza_d <- function(palette = "margherita", discrete = TRUE, reverse = FALSE, ...) {
  scale_fill_pizza(palette = palette, discrete = discrete, reverse = reverse, ...)
}

#' @rdname scale_color_pizza
#' @export
scale_fill_pizza_c <- function(palette = "margherita", discrete = FALSE, reverse = FALSE, ...) {
  scale_fill_pizza(palette = palette, discrete = discrete, reverse = reverse, ...)
}




# Palette --------------------------------------------------------------------



# The palette based on this image: https://www.scattidigusto.it/wp-content/uploads/2018/03/pizza-margherita-originale-Scatti-di-Gusto.jpg
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
#' @param palette Pizza type. Can be "margherita" (default) or "diavola".
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments to pass to \code{\link[=colorRampPalette]{colorRampPalette()}}.
#'
#' @importFrom grDevices colorRampPalette
#' @export
palette_pizza <- function(palette = "margherita", reverse = FALSE, ...) {
  pal <- pizza_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

