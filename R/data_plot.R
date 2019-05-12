#' Prepare an object for plotting
#'
#' This function attempts to extract and tranform an object to be further plotted.
#'
#' @param x An object.
#' @param data The original data used to create this object. Can be a statistical model or such.
#' @param rope_alpha Transparency level of ROPE ribbon.
#' @param rope_color Color of ROPE ribbon.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(bayestestR)
#'
#' data <- rnorm(1000, 1)
#' x <- rope(data, ci = c(0.8, 0.9))
#' plot(x)
#'
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris)
#' x <- rope(model)
#' plot(x)
#' }
#'
#' data <- rnorm(1000, 1)
#' x <- hdi(data, c(0.8, 0.9))
#' plot(x) + theme_modern()
#'
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris)
#' x <- hdi(model)
#' plot(x) + theme_modern()
#' }
#'
#' data <- rnorm(1000, 1)
#' x <- p_direction(data)
#' plot(x)
#'
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris)
#' x <- p_direction(model)
#' plot(x)
#' }
#'
#' @export
data_plot <- function(x, data = NULL, ...){
  UseMethod("data_plot")
}



#' @method print data_plot
#' @importFrom graphics plot
#' @export
print.data_plot <- function(x, ...){
  print(as.data.frame(x))
}



#' Add information stored as attributes on plot
#'
#' @inheritParams data_plot
#' @export
add_plot_attributes <- function(x){
  info <- attributes(x)$info
  out <- list(ylab(info$ylab), xlab(info$xlab))

  if (!is.null(info$legend_fill)) {
    out[[length(out) + 1]] <- labs(fill = info$legend_fill)
  }
  if (!is.null(info$legend_color)) {
    out[[length(out) + 1]] <- labs(color = info$legend_color)
  }
  if (!is.null(info$title)) {
    out[[length(out) + 1]] <- labs(title = info$title)
  }

  out
}












#' @keywords internal
.retrieve_data <- function(x){
  # retrieve model
  data <- tryCatch(
    {
      if (!is.null(attr(x, "object_name", exact = TRUE)))
        get(attributes(x)$object_name, envir = parent.frame())
      else
        attr(x, "data", exact = TRUE)
    },
    error = function(e) { NULL }
  )

  if (is.null(data)) {
    stop("Failed at retrieving data :( Please provide original model or data through the `data` argument", call. = FALSE)
  }

  data
}