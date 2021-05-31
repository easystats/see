#' Create ggplot2 geom(s) from a list
#'
#' These helper functions are built on top of \code{ggplot2::layer()} and can be used
#' to add geom(s) which type and content is specified as a list.
#'
#' @param x A list containing a geom type (e.g., \code{geom = "point"}), a list of aesthetics (as characters; e.g., \code{aes = list(x = "mpg", y = "wt")}), some data (e.g., \code{data = mtcars}) and some other parameters. For \code{geoms_from_list()} ("geoms" with an "s"), the input must be a list of lists, ideally named \code{"l1", "l2", "l3"}, etc.
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
#' ggplot() +
#'   geoms_from_list(list(l1 = l1, l2 = l2))
#'
#'
#' @export
geom_from_list <- function(x, ...) {

  # Separate additional parameters
  args <- x[!names(x) %in% c("geom", "aes", "data")]

  # If labs, return immediately
  if(x$geom == "labs") return(do.call(ggplot2::labs, args))

  # Fix for geom_jitter (because geom cannot be 'jitter')
  if(x$geom == "jitter") {
    x$geom <- "point"
    position <- "jitter"
  } else {
    position <- "identity"
  }

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


#' @export
geoms_from_list <- function(x, ...) {

  # Get name of layers
  n <- length(x)
  l_names <- paste0("l", 1:n)
  if(!all(l_names %in% names(x))) l_names <- names(x)

  layers <- list()
  for(i in l_names) {
    layers[[i]] <- geom_from_list(x[[i]])
  }
  layers
}