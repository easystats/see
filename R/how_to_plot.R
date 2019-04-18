#' How to plot your object
#'
#' Access the source code used for plotting.
#'
#' @param x An object.
#' @param ... Arguments passed to or from other methods.
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

  # We'll reactivate this argument if we find a purpose for it :)
  # if (source != TRUE) {
  #   warning("Be sure to set 'source = TRUE' to access source code.")
  # }
}





#' @rdname how_to_plot
#' @export
how_to_plot.hdi  <- function(x, ...) {
  cat(.how_to_plode_cleaner("plot.hdi"))
}

#' @rdname how_to_plot
#' @export
how_to_plot.p_direction  <- function(x, ...) {
  cat(.how_to_plode_cleaner("plot.p_direction"))
}






#' @importFrom utils head tail getAnywhere
#' @keywords internal
.how_to_plode_cleaner <- function(name){
  sourcecode <- getAnywhere(name)
  code <- sourcecode$objs[sourcecode$dups == FALSE]
  code <- as.character(code)
  # Split by line
  code <- strsplit(code, "\n", fixed=TRUE)[[1]]

  # Remove beginning and end of function
  code <- tail(head(code, -2), -5)
  # Replace begining
  code <- gsub("p <- x", "data_plot(x)", code, fixed=TRUE)
  # Split
  code <- gsub("+", "+\n", code, fixed=TRUE)

  code <- paste0("Assuming that the input object is `x`:",
                 "\n\n",
                 code)
  return(code)

}

