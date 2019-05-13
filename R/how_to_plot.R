#' How to plot your object
#'
#' Access the source code used for plotting.
#'
#' @param x An object.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(bayestestR)
#' results <- hdi(rnorm(1000))
#' how_to_plot(results)
#' @export
how_to_plot  <- function(x, ...) {
  UseMethod("how_to_plot")
}


#' @export
how_to_plot.hdi  <- function(x, ...) {
  cat(.how_to_plode_cleaner("plot.see_hdi"))
}


#' @export
how_to_plot.ci  <- function(x, ...) {
  cat(.how_to_plode_cleaner("plot.see_ci"))
}


#' @export
how_to_plot.p_direction  <- function(x, ...) {
  cat(.how_to_plode_cleaner("plot.see_p_direction"))
}


#' @export
how_to_plot.rope  <- function(x, ...) {
  cat(.how_to_plode_cleaner("plot.see_rope"))
}





#' @importFrom utils head tail getAnywhere
#' @keywords internal
.how_to_plode_cleaner <- function(name){
  sourcecode <- utils::getAnywhere(name)
  code <- sourcecode$objs[sourcecode$dups == FALSE]
  code <- as.character(code)
  # Split by line
  code <- strsplit(code, "\n", fixed = TRUE)[[1]]

  # Remove beginning and end of function
  code <- utils::tail(utils::head(code, -2), -5)
  # Replace begining
  code <- gsub("p <- x", "data_plot(x)", code, fixed = TRUE)
  # Split
  code <- gsub("+", "+\n ", code, fixed = TRUE)
  code <- gsub("%>%", "%>%\n ", code, fixed = TRUE)

  paste0("Assuming that the input object is `x`:\n\n", trimws(code))
}

