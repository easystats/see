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
#'
#' library(bayestestR)
#' library(see)
#'
#' data <- rnorm(1000, 1)
#'
#' x <- rope(data)
#' dataplot <- data_plot(x, data)
#' plot(dataplot)
#'
#' x <- rope(data, ci=c(0.8, 0.9))
#' dataplot <- data_plot(x, data)
#' plot(dataplot)
#'
#' \dontrun{
#' library(rstanarm)
#' data <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' x <- rope(data)
#' dataplot <- data_plot(x, data)
#' plot(dataplot)
#'
#' x <- rope(data, ci=c(0.8, 0.9))
#' dataplot <- data_plot(x, data)
#' plot(dataplot)
#' }
#'
#' library(bayestestR)
#' data <- rnorm(1000, 1)
#' x <- rope(data, ci = c(0.8, 0.9))
#'
#' plot(x, data) +
#'   theme_modern()
#'
#' \dontrun{
#' data <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' x <- rope(data, ci = c(0.8, 0.9))
#'
#' plot(x, data) +
#'   theme_modern()
#' }
#'
#' library(bayestestR)
#' library(see)
#'
#' data <- rnorm(1000, 1)
#' x <- hdi(data, c(0.8, 0.9))
#' data <- data_plot(x, data)
#' plot(data)
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' x <- hdi(model)
#' data <- data_plot(x)
#' plot(data)
#' }
#'
#' \dontrun{
#' library(bayestestR)
#' data <- rnorm(1000, 1)
#' x <- hdi(data, ci=c(0.8, 0.9))
#'
#' plot(x, data) +
#'   theme_modern()
#'
#' }
#'
#' \dontrun{
#' library(rstanarm)
#' library(estimate)
#'
#' model <- stan_glm(Sepal.Width ~ Species, data=iris)
#'
#' contrasts <- estimate_contrasts(model, test=NULL)
#' means <- estimate_means(model)
#'
#' data <- data_plot(contrasts, means)
#'
#' plot(data)
#' }
#'
#' library(bayestestR)
#' library(see)
#'
#' data <- rnorm(1000, 1)
#' x <- p_direction(data)
#' data <- data_plot(x, data)
#' plot(data)
#'
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' x <- p_direction(model)
#' data <- data_plot(x)
#' plot(data)
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
  if(!is.null(info$title)){
    out[[length(out)+1]] <- labs(title=info$title)
  }

  out
}












#' @keywords internal
.retrieve_data <- function(x){
  # retrieve model
  data <- tryCatch(
    {
      get(attributes(x)$object_name, envir = parent.frame())
    },
    error = function(e) { NULL }
  )

  if (is.null(data)) {
    stop("Failed at retrieving data :( Please provide original model or data through the `data` argument", call. = FALSE)
  }

  data
}