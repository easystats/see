#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


.as.data.frame_density <- function(x, ...) {
  data.frame(x = x$x, y = x$y)
}



.remove_intercept <- function(x, column = "Parameter", show_intercept) {
  if (!show_intercept) {
    remove <- which(x[[column]] %in% c("Intercept", "(Intercept)", "b_Intercept"))
    if (length(remove)) x <- x[-remove, ]
  }
  x
}



.normalize <- function(x) {
  as.vector((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE), na.rm = TRUE))
}



.compact_list <- function(x) {
  if (!is.null(x) && length(x) > 0 && is.list(x)) {
    x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]
  } else {
    x
  }
}
