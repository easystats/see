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
#' # Example 1
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
#' # Example 2
#' l1 <- list(geom = "violin",
#'            data = iris,
#'            aes = list(x = "Species", y = "Sepal.Width"))
#' l2 <- list(geom = "boxplot",
#'            data = iris,
#'            aes = list(x = "Species", y = "Sepal.Width"),
#'            outlier.shape = NA)
#' l3 <- list(geom = "jitter",
#'            data = iris,
#'            width = 0.1,
#'            aes = list(x = "Species", y = "Sepal.Width"))
#'
#' ggplot() +
#'   geom_from_list(l1) +
#'   geom_from_list(l2)  +
#'   geom_from_list(l3)
#'
#' # Example 3
#' ggplot() +
#'   geom_from_list(list(geom = "density_2d", data = iris,
#'                       aes = list(x = "Sepal.Width", y = "Petal.Length")))
#' ggplot() +
#'   geom_from_list(list(geom = "density_2d_filled", data = iris,
#'                       aes = list(x = "Sepal.Width", y = "Petal.Length")))
#' ggplot() +
#'   geom_from_list(list(geom = "density_2d_polygon", data = iris,
#'                       aes = list(x = "Sepal.Width", y = "Petal.Length")))
#'
#' ggplot() +
#'   geom_from_list(list(geom = "density_2d_raster", data = iris,
#'                       aes = list(x = "Sepal.Width", y = "Petal.Length"))) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_y_continuous(expand = c(0, 0))
#' @export
geom_from_list <- function(x, ...) {

  # Additional parameters ------------------------------------------------------
  args <- x[!names(x) %in% c("geom", "aes", "data", "width", "height", "position")]

  if(x$geom %in% c("density_2d", "density_2d_filled", "density_2d_polygon")) {
    if(!"contour" %in% names(args)) args$contour <- TRUE
    if(!"contour_var" %in% names(args)) args$contour_var <- "density"
  }

  # If labs, return immediately
  if(x$geom == "labs") return(do.call(ggplot2::labs, args))

  # Default parameters
  stat <- "identity"
  position <- "identity"

  # Fix for geom_jitter (because geom cannot be 'jitter')
  if(x$geom == "jitter") {
    x$geom <- "point"
    position <- ggplot2::position_jitter(width = x$width, height = x$height)
  }

  # Default for violin
  if(x$geom == "violin") {
    stat <- "ydensity"
    position <- "dodge"
  } else if(x$geom == "boxplot") {
    stat <- "boxplot"
    position <- "dodge2"
  } else if(x$geom == "density_2d"){
    stat <- ggplot2::StatDensity2d
  } else if(x$geom == "density_2d_filled") {
    stat <- ggplot2::StatDensity2dFilled
  } else if(x$geom == "density_2d_polygon") {
    stat <- ggplot2::StatDensity2d
    x$geom <- "polygon"
    if(!"fill" %in% names(x$aes)) x$aes$fill <- "..level.."
  } else if(x$geom == "density_2d_raster") {
    stat <- ggplot2::StatDensity2d
    x$geom <- "raster"
    if(!"fill" %in% names(x$aes)) x$aes$fill <- "..density.."
  }

  # Position
  if("position" %in% names(x)) {
    if(is.character(x$position) && x$position == "dodge") {
      position <- ggplot2::position_dodge(width = x$width)
    } else if (is.character(x$position) && x$position == "jitter") {
      position <- ggplot2::position_jitter(width = x$width, height = x$height)
    } else {
      position <- x$position
    }
  }

  # Aesthetics
  aes_list <- do.call(ggplot2::aes_string, x$aes)

  # Create layer
  ggplot2::layer(stat = stat,
                 position = position,
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