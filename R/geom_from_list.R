#' Create ggplot2 geom(s) from a list
#'
#' These helper functions are built on top of `ggplot2::layer()` and can be
#' used to add geom(s), whose type and content are specified as a list.
#'
#' @param x A list containing:
#' - a geom type (e.g. `geom = "point"`),
#' - a list of aesthetics (e.g. `aes = list(x = "mpg", y = "wt")`),
#' - some data (e.g. `data = mtcars`),
#' - and some other parameters.
#'
#' For `geoms_from_list()` ("geoms" with an "s"), the input must be a list of
#' lists, ideally named `"l1", "l2", "l3"`, etc.
#' @param ... Additional arguments passed to `ggplot2::layer()`.
#'
#'
#' @examplesIf require("ggside") && require("ggplot2")
#' library(ggplot2)
#'
#' # Example 1 (basic geoms and labels) --------------------------
#' l1 <- list(
#'   geom = "point",
#'   data = mtcars,
#'   aes = list(x = "mpg", y = "wt", size = "hp", color = "hp"),
#'   show.legend = c("size" = FALSE)
#' )
#' l2 <- list(
#'   geom = "labs",
#'   title = "A Title"
#' )
#'
#' ggplot() +
#'   geom_from_list(l1) +
#'   geom_from_list(l2)
#'
#' ggplot() +
#'   geoms_from_list(list(l1 = l1, l2 = l2))
#'
#' # Example 2 (Violin, boxplots, ...) --------------------------
#' l1 <- list(
#'   geom = "violin",
#'   data = iris,
#'   aes = list(x = "Species", y = "Sepal.Width")
#' )
#' l2 <- list(
#'   geom = "boxplot",
#'   data = iris,
#'   aes = list(x = "Species", y = "Sepal.Width"),
#'   outlier.shape = NA
#' )
#' l3 <- list(
#'   geom = "jitter",
#'   data = iris,
#'   width = 0.1,
#'   aes = list(x = "Species", y = "Sepal.Width")
#' )
#'
#' ggplot() +
#'   geom_from_list(l1) +
#'   geom_from_list(l2) +
#'   geom_from_list(l3)
#'
#' # Example 3 (2D density) --------------------------
#' ggplot() +
#'   geom_from_list(list(
#'     geom = "density_2d", data = iris,
#'     aes = list(x = "Sepal.Width", y = "Petal.Length")
#'   ))
#' ggplot() +
#'   geom_from_list(list(
#'     geom = "density_2d_filled", data = iris,
#'     aes = list(x = "Sepal.Width", y = "Petal.Length")
#'   ))
#' ggplot() +
#'   geom_from_list(list(
#'     geom = "density_2d_polygon", data = iris,
#'     aes = list(x = "Sepal.Width", y = "Petal.Length")
#'   ))
#' ggplot() +
#'   geom_from_list(list(
#'     geom = "density_2d_raster", data = iris,
#'     aes = list(x = "Sepal.Width", y = "Petal.Length")
#'   )) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   scale_y_continuous(expand = c(0, 0))
#'
#' # Example 4 (facet and coord flip) --------------------------
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) +
#'   geom_point() +
#'   geom_from_list(list(geom = "hline", yintercept = 2)) +
#'   geom_from_list(list(geom = "coord_flip")) +
#'   geom_from_list(list(geom = "facet_wrap", facets = "~ Species", scales = "free"))
#'
#' # Example 5 (theme and scales) --------------------------
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, color = Species)) +
#'   geom_point() +
#'   geom_from_list(list(geom = "scale_color_viridis_d", option = "inferno")) +
#'   geom_from_list(list(geom = "theme", legend.position = "top"))
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, color = Species)) +
#'   geom_point() +
#'   geom_from_list(list(geom = "scale_color_material_d", palette = "rainbow")) +
#'   geom_from_list(list(geom = "theme_void"))
#'
#' # Example 5 (Smooths and side densities) --------------------------
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) +
#'   geom_from_list(list(geom = "point")) +
#'   geom_from_list(list(geom = "smooth", color = "red")) +
#'   geom_from_list(list(aes = list(x = "Sepal.Length"), geom = "ggside::geom_xsidedensity")) +
#'   geom_from_list(list(geom = "ggside::scale_xsidey_continuous", breaks = NULL))
#'
#' @export
geom_from_list <- function(x, ...) {
  # Additional parameters ------------------------------------------------------
  args <- x[!names(x) %in% c("geom", "aes", "data", "width", "height", "position", "show.legend")]

  if (is.null(x$geom)) {
    return(NULL)
  }

  if (inherits(x$geom, "function")) {
    return(do.call(x$geom, args))
  }

  if (x$geom %in% c("density_2d", "density_2d_filled", "density_2d_polygon")) {
    if (!"contour" %in% names(args)) args$contour <- TRUE
    if (!"contour_var" %in% names(args)) args$contour_var <- "density"
  }

  # If they are not geoms, return immediately
  if (x$geom == "labs") {
    return(do.call(ggplot2::labs, args))
  }
  if (x$geom == "guides") {
    return(do.call(ggplot2::guides, args))
  }
  if (x$geom == "coord_flip") {
    return(do.call(ggplot2::coord_flip, args))
  }
  if (x$geom == "facet_wrap") {
    return(do.call(ggplot2::facet_wrap, args))
  }
  if (x$geom == "facet_grid") {
    return(do.call(ggplot2::facet_grid, args))
  }
  if (x$geom == "smooth") {
    if (!is.null(x$aes)) args$mapping <- do.call(ggplot2::aes, lapply(x$aes, .str_to_sym))
    if (!"method" %in% names(args)) args$method <- "loess"
    if (!"formula" %in% names(args)) args$formula <- "y ~ x"
    return(do.call(ggplot2::geom_smooth, args))
  }

  if (startsWith(x$geom, "scale_") || startsWith(x$geom, "theme") || startsWith(x$geom, "see_")) {
    return(do.call(x$geom, args))
  }

  if (startsWith(x$geom, "ggside::")) {
    insight::check_if_installed("ggside")
    if (!is.null(x$aes)) args$mapping <- do.call(ggplot2::aes, lapply(x$aes, .str_to_sym))
    return(do.call(eval(parse(text = x$geom)), args))
  }

  if (startsWith(x$geom, "ggraph::")) {
    insight::check_if_installed("ggraph")
    if (!is.null(x$aes)) args$mapping <- do.call(ggplot2::aes, lapply(x$aes, .str_to_sym))
    return(do.call(eval(parse(text = x$geom)), args))
  }

  # Default parameters
  stat <- "identity"
  position <- "identity"

  # Fix for geom_jitter (because geom cannot be 'jitter')
  if (x$geom == "jitter") {
    x$geom <- "point"
    position <- ggplot2::position_jitter(width = x$width, height = x$height)
  }

  # Default for violin
  if (x$geom == "violin") {
    stat <- "ydensity"
    position <- "dodge"
  } else if (x$geom == "boxplot") {
    stat <- "boxplot"
    position <- "dodge2"
  } else if (x$geom == "density_2d") {
    stat <- ggplot2::StatDensity2d
  } else if (x$geom == "density_2d_filled") {
    stat <- ggplot2::StatDensity2dFilled
  } else if (x$geom == "density_2d_polygon") {
    stat <- ggplot2::StatDensity2d
    x$geom <- "polygon"
    if (!"fill" %in% names(x$aes)) x$aes$fill <- quote(after_stat(level))
  } else if (x$geom == "density_2d_raster") {
    stat <- ggplot2::StatDensity2d
    x$geom <- "raster"
    if (!"fill" %in% names(x$aes)) x$aes$fill <- quote(after_stat(density))
  }

  # Position
  if ("position" %in% names(x)) {
    if (is.character(x$position) && x$position == "dodge") {
      position <- ggplot2::position_dodge(width = x$width)
    } else if (is.character(x$position) && x$position == "jitter") {
      position <- ggplot2::position_jitter(width = x$width, height = x$height)
    } else {
      position <- x$position
    }
  }

  # Aesthetics
  if ("aes" %in% names(x)) {
    aes_list <- do.call(ggplot2::aes, lapply(x$aes, .str_to_sym))
  } else {
    aes_list <- NULL
  }

  # Show.legend
  if ("show.legend" %in% names(x)) {
    show.legend <- x$show.legend
  } else {
    show.legend <- NA
  }

  # Create layer
  ggplot2::layer(
    stat = stat,
    position = position,
    geom = x$geom,
    mapping = aes_list,
    data = x$data,
    params = args,
    show.legend = show.legend,
    ...
  )
}

#' @rdname geom_from_list
#' @export
geoms_from_list <- function(x, ...) {
  # Get name of layers
  n <- length(x)
  l_names <- paste0("l", 1:n)
  if (!all(l_names %in% names(x))) l_names <- names(x)

  layers <- list()
  for (i in l_names) {
    layers[[i]] <- geom_from_list(x[[i]])
  }
  layers
}
