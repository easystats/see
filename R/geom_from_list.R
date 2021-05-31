#' Create a ggplot2 geom from a list
#'
#' This helper function is built on top of \code{ggplot2::layer()} and can be used
#' to add a geom which type and content is specified as a list.
#'
#' @param x A list containing a geom type (e.g., \code{geom = "point"}), a list of aesthetics (as characters; e.g., \code{aes = list(x = "mpg", y = "wt")}), some data (e.g., \code{data = mtcars}) and some other parameters.
#'
#' @examples
#' library(ggplot2)
#'
#' l1 <- list(geom = "point",
#'            data = mtcars,
#'            aes = list(x = "mpg", y = "wt"))
#' l2 <- list(geom = "labs",
#'            title = "A Title")
#'
#' ggplot() +
#'   geom_from_list(l1) +
#'   geom_from_list(l2)
#'
#' @export
geom_from_list <- function(x, ...) {

  # Separate additional parameters
  args <- x[!names(x) %in% c("geom", "aes", "data")]

  # If labs, return immediately
  if(x$geom == "labs") return(do.call(ggplot2::labs, args))

  # Aesthetics
  aes_list <- do.call(ggplot2::aes_string, x$aes)

  # Create layer
  ggplot2::layer(stat = "identity",
                 position = "identity",
                 geom = x$geom,
                 mapping = aes_list,
                 data = x$data,
                 params = args,
                 ...)
}