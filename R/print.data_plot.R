#' @method print data_plot
#' @export
print.data_plot <- function(x, ...) {
  print(as.data.frame(x))
  invisible(x)
}
