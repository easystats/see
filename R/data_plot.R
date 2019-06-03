#' @title Prepare objects for plotting or plot objects
#' @name data_plot
#'
#' @description \code{data_plot()} attempts to extract and tranform an object
#' to be further plotted, while \code{plot()} tries to visualize results of
#' functions from different packages of the \href{https://github.com/easystats}{easystats-project}.
#'
#' @param x An object.
#' @param data The original data used to create this object. Can be a statistical model or such.
#' @param show_intercept Logical, if \code{TRUE}, the intercept-parameter is included
#'   in the plot. By default, it is hidden because in many cases the intercept-parameter
#'   has a posterior distribution on a very different location, so density curves of
#'   posterior distributions for other parameters are hardly visible.
#' @param ... Arguments passed to or from other methods.
#'
#' @details \code{data_plot()} is in most situation not needed when the purpose
#' is plotting, since most \code{plot()}-functions in \pkg{see} internally call
#' \code{data_plot()} to prepare the data for plotting.
#' \cr \cr
#' Many \code{plot()}-functions have a \code{data}-argument that is needed when
#' the data or model for plotting can't be retrieved via \code{data_plot()}. In
#' such cases, \code{plot()} gives an error and asks for providing data or models.
#' \cr \cr
#' Most \code{plot()}-functions work out-of-the-box, i.e. you don't need to do
#' much more than calling \code{plot(<object>)} (see 'Examples'). Some plot-functions
#' allow to specify arguments to modify the transparancy or color of geoms, these
#' are shown in the 'Usage' section.
#' \cr \cr
#' Plot-functions are available for objects from following functions (note that
#' functions from packages might be listed here that are currently still in
#' development and probably not yet available):
#' \itemize{
#'   \item \code{bayestestR::bayesfactor_models()}
#'   \item \code{bayestestR::bayesfactor_savagedickey()}
#'   \item \code{bayestestR::ci()}
#'   \item \code{bayestestR::equivalence_test()}
#'   \item \code{bayestestR::hdi()}
#'   \item \code{bayestestR::p_direction()}
#'   \item \code{bayestestR::rope()}
#'   \item \code{estimate::estimateContrasts()}
#'   \item \code{performance::binned_residuals()}
#'   \item \code{performance::roc()}
#' }
#'
#' @examples
#' library(bayestestR)
#' library(rstanarm)
#'
#' data <- rnorm(1000, 1)
#' x <- rope(data, ci = c(0.8, 0.9))
#' plot(x)
#'
#' model <- stan_glm(
#'   Sepal.Length ~ Petal.Width * Species,
#'   data = iris,
#'   chains = 2, iter = 200
#' )
#'
#' x <- rope(model)
#' plot(x)
#'
#' data <- rnorm(1000, 1)
#' x <- hdi(data, c(0.8, 0.9))
#' plot(x) + theme_modern()
#'
#' x <- hdi(model)
#' plot(x) + theme_modern()
#'
#' data <- rnorm(1000, 1)
#' x <- p_direction(data)
#' plot(x)
#'
#' x <- p_direction(model)
#' plot(x)
#'
#' model <- stan_glm(
#'   mpg ~ wt + gear + cyl + disp,
#'   chains = 2,
#'   iter = 200,
#'   data = mtcars
#' )
#' x <- equivalence_test(model)
#' plot(x)
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



#' Complete figure with its attributes
#'
#' The \link{data_plot} function usually stores information (such as title, axes labels etc.) as attributes. This function adds those information to the plot.
#'
#' @inheritParams data_plot
#' @examples
#' library(rstanarm)
#' library(bayestestR)
#' library(see)
#' library(ggplot2)
#'
#' model <- stan_glm(
#'   Sepal.Length ~ Petal.Width + Species + Sepal.Width,
#'   data = iris,
#'   chains = 2, iter = 200
#' )
#'
#' result <- hdi(model, ci = c(0.5, 0.75, 0.9, 0.95))
#' data <- data_plot(result, data = model)
#'
#' p <- data %>%
#'   ggplot(aes(x = x, y = y, height = height, group = y, fill = fill)) +
#'   ggridges::geom_ridgeline_gradient()
#'
#' p
#' p + add_plot_attributes(data)
#'
#'
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