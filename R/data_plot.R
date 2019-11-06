#' @title Prepare objects for plotting or plot objects
#' @name data_plot
#'
#' @description \code{data_plot()} attempts to extract and tranform an object
#' to be further plotted, while \code{plot()} tries to visualize results of
#' functions from different packages of the \href{https://github.com/easystats}{easystats-project}.
#'
#' @param x An object.
#' @param data The original data used to create this object. Can be a
#'   statistical model or such.
#' @param show_intercept Logical, if \code{TRUE}, the intercept-parameter is included
#'   in the plot. By default, it is hidden because in many cases the intercept-parameter
#'   has a posterior distribution on a very different location, so density curves of
#'   posterior distributions for other parameters are hardly visible.
#' @param show_labels Logical, if \code{TRUE}, the text labels for the point
#'   estimates (i.e. \emph{"Mean"}, \emph{"Median"} and/or \emph{"MAP"}) are shown.
#'   You may set \code{show_labels = FALSE} in case of overlapping labels, and
#'   add your own legend or footnote to the plot.
#' @param priors Logical, if \code{TRUE}, prior distributions are simulated
#'   (using \code{\link[bayestestR]{simulate_prior}}) and added to the plot.
#' @param priors_alpha Alpha value of the prior distributions.
#' @param point_size Size of point-geoms.
#' @param rope_alpha Transparency level of ROPE ribbon.
#' @param rope_color Color of ROPE ribbon.
#' @param n_columns For models with multiple components (like fixed and random, count and zero-inflated), defines the number of columns for the panel-layout. If \code{NULL}, a single, integrated plot is shown.
#' @param stack Logical, if \code{TRUE}, densities are plotted as stacked lines.
#'   Else, densities are plotted for each parameter among each other.
#' @param n_pies Number of pies.
#' @param value What value to display.
#' @param log Show log-transformed Bayes factors.
#' @param text_size Size of text labels.
#' @param text_color Color of text labels.
#' @param threshold_coefficient Numeric, threshold at which value coefficients will be displayed.
#' @param threshold_p Numeric, threshold at which value p-values will be displayed.
#' @param ci Logical, whether confidence intervals should be added to the plot.
#' @param size Size of geoms. Ddepends on the context of the \code{plot()} function,
#'   so this argument may change size of points, lines or bars.
#' @param panel Logical, if \code{TRUE}, plots are arranged as panels; else,
#'   single plots are returned.
#' @param type Character vector, indicating the type of plot (for
#'   \code{\link[performance]{check_normality}}, \code{parameters::model_parameters.lavaan}
#'   or \code{\link[parameters]{n_factors}}).
#' @param prior_odds optional vector of prior odds for the models. See
#'   \code{BayesFactor::priorOdds}. As the size of the pizza slices corresponds
#'   to posterior probability (which is a function of prior probability and the BF),
#'   custom \code{prior_odds} will change the slices' size.
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
#'   \item \code{bayestestR::bayesfactor_parameters()}
#'   \item \code{bayestestR::bayesfactor_savagedickey()}
#'   \item \code{bayestestR::ci()}
#'   \item \code{bayestestR::equivalence_test()}
#'   \item \code{bayestestR::estimate_density()}
#'   \item \code{bayestestR::eti()}
#'   \item \code{bayestestR::hdi()}
#'   \item \code{bayestestR::map_estimate()}
#'   \item \code{bayestestR::p_direction()}
#'   \item \code{bayestestR::p_significance()}
#'   \item \code{bayestestR::point_estimate()}
#'   \item \code{bayestestR::rope()}
#'   \item \code{estimate::estimate_contrasts()}
#'   \item \code{parameters::model_parameters()}
#'   \item \code{parameters::n_factors()}
#'   \item \code{parameters::parameters_simulate()}
#'   \item \code{parameters::principal_components()}
#'   \item \code{performance::binned_residuals()}
#'   \item \code{performance::check_collinearity()}
#'   \item \code{performance::check_distribution()}
#'   \item \code{performance::check_heteroscedasticity()}
#'   \item \code{performance::check_homogeneity()}
#'   \item \code{performance::check_model()}
#'   \item \code{performance::check_normality()}
#'   \item \code{performance::check_outliers()}
#'   \item \code{performance::performance_roc()}
#' }
#'
#' @seealso \href{https://easystats.github.io/see/articles/}{Package-Vignettes}
#'
#' @examples
#' \dontrun{
#' library(bayestestR)
#' library(rstanarm)
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
#' plot(x)}
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
#' \dontrun{
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
#' p + add_plot_attributes(data)}
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
.retrieve_data <- function(x) {
  # retrieve model
  obj_name <- attr(x, "object_name", exact = TRUE)
  dat <- NULL

  if (!is.null(obj_name)) {
    # first try, parent frame
    dat <- tryCatch({
      get(obj_name, envir = parent.frame())
    },
    error = function(e) { NULL }
    )

    if (is.null(dat)) {
      # second try, global env
      dat <- tryCatch({
        get(obj_name, envir = globalenv())
      },
      error = function(e) { NULL }
      )

    }
  }

  if (is.null(dat)) {
    dat <- attr(x, "data", exact = TRUE)
  }


  if (is.null(dat)) {
    stop("Failed at retrieving data :( Please provide original model or data through the `data` argument", call. = FALSE)
  }

  dat
}