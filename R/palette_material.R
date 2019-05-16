# The palette based on material design colors: https://www.materialui.co/colors
material_colors_list <- c(
  `red` = "#f44336",
  `pink` = "#E91E63",
  `purple` = "#9C27B0",
  `deep purple` = "#673AB7",
  `indigo` = "#3F51B5",
  `blue` = "#2196F3",
  `light blue` = "#03A9F4",
  `cyan` = "#00BCD4",
  `teal` = "#009688",
  `green` = "#4CAF50",
  `light green` = "#8BC34A",
  `lime` = "#CDDC39",
  `yellow` = "#FFEB3B",
  `amber` = "#FFC107",
  `orange` = "#FF9800",
  `deep orange` = "#FF5722",
  `brown` = "#795548",
  `grey` = "#9E9E9E",
  `blue grey` = "#607D8B"
)


#' Extract material design colors as hex codes
#'
#' Can be used to get the hex code of specific colors from the material design color palette. Use \code{material_colors()} to see all available color.
#'
#' @param ... Character names of \href{https://www.materialui.co/colors}{material design colors}.
#'
#' @examples
#' material_colors()
#'
#' material_colors("indigo", "lime")
#'
#' @export
material_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(material_colors_list)
  }

  material_colors_list[cols]
}




material_palettes <- list(
  `full`  = material_colors(),
  `ice`  = material_colors("purple", "deep purple", "indigo", "blue", "light blue"),
  `rainbow` = material_colors("purple", "deep purple", "indigo", "blue", "light blue", "green", "light green", "lime", "amber", "orange", "deep orange" ,"red", "pink"),
  `contrast` = material_colors("blue", "green", "amber", "purple", "red")
)






#' Material design color palette
#'
#' The palette based on material design colors (https://www.materialui.co/colors).
#'
#' @param palette Character name of palette in material_palettes. Can be "full", "ice", "rainbow" or "contrast" (default).
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments to pass to \code{\link[=colorRampPalette]{colorRampPalette()}}.
#'
#' @importFrom grDevices colorRampPalette
#' @export
palette_material <- function(palette = "contrast", reverse = FALSE, ...) {
  pal <- material_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


