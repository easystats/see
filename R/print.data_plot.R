#' @method print data_plot
#' @importFrom graphics plot
#' @export
print.data_plot <- function(x, ...){
  orig_x <- x
  print(as.data.frame(x))
  invisible(orig_x)
}