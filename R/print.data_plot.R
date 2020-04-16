#' @method print data_plot
#' @importFrom graphics plot
#' @export
print.data_plot <- function(x, ...){
  print(as.data.frame(x))
  invisible(x)
}