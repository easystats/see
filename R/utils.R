#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @keywords internal
.as.data.frame_density <- function(x, ...) {
  data.frame(x = x$x, y = x$y)
}