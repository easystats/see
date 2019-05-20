#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @keywords internal
.as.data.frame_density <- function(x, ...) {
  data.frame(x = x$x, y = x$y)
}



#' @keywords internal
.remove_intercept <- function(x, column = "Parameter", show_intercept) {
  if (!show_intercept) {
    remove <- which(x[[column]] %in% c("Intercept", "(Intercept)", "b_Intercept"))
    if (length(remove)) x <- x[-remove, ]
  }
  x
}