#' How to plot your object
#'
#' Access the source code used for plotting.
#'
#' @param x An object.
#' @param ... Arguments passed to or from other methods.
#'
#' @details The plotting-capability of the \pkg{see} package mainly evolve around
#' two functions: \code{\link[=data_plot]{data_plot()}}, which prepares the data from various
#' objects to bring it into shape for plotting, and \code{\link[=data_plot]{plot()}},
#' which takes the data from \code{data_plot()} and creates the ggplot-object.
#' Although ggplot-objects are easily modifiable, it is sometimes necessary to
#' build up a plot from scratch, using the data that should be plotted. This
#' is where \code{how_to_plot()} can help. It simply extracts and polishes the
#' code from the various \code{plot()} methods and prints it to the console.
#' This code can be used as "basis" for building own ggplots.
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#' results <- hdi(rnorm(1000))
#'
#' how_to_plot(results)
#' }
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





#' @importFrom insight print_color
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
  # Remove .data$
  code <- gsub(".data$", "", code, fixed = TRUE)
  # Split
  code <- gsub("+", "+\n ", code, fixed = TRUE)
  code <- gsub("%>%", "%>%\n ", code, fixed = TRUE)

  insight::print_color("# Assuming that the input object is `x`:\n\n", "cyan")
  cat(trimws(code))
}

