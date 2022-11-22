#' @title Prepare objects for plotting or plot objects
#' @name data_plot
#'
#' @description `data_plot()` extracts and transforms an object for plotting,
#'   while `plot()` visualizes results of functions from different packages in
#'   [easystats-project](https://github.com/easystats). See the documentation
#'   for your object's class:
#' \itemize{
#'  \item{\link[=plot.see_bayesfactor_models]{bayestestR::bayesfactor_models()}}
#'  \item{\link[=plot.see_bayesfactor_parameters]{bayestestR::bayesfactor_parameters()}}
#'  \item{\link[=plot.see_equivalence_test]{bayestestR::equivalence_test()}}
#'  \item{\link[=plot.see_estimate_density]{bayestestR::estimate_density()}}
#'  \item{\link[=plot.see_hdi]{bayestestR::hdi()}}
#'  \item{\link[=plot.see_p_direction]{bayestestR::p_direction()}}
#'  \item{\link[=plot.see_p_significance]{bayestestR::p_significance()}}
#'  \item{\link[=plot.see_si]{bayestestR::si()}}
#'  \item{\link[=plot.see_effectsize_table]{effectsize::effectsize()}}
#'  \item{\link[=plot.see_estimate_contrasts]{modelbased::estimate_contrasts()}}
#'  \item{\link[=plot.see_compare_parameters]{parameters::compare_parameters()}}
#'  \item{\link[=plot.see_parameters_distribution]{parameters::describe_distribution()}}
#'  \item{\link[=plot.see_parameters_model]{parameters::model_parameters()}}
#'  \item{\link[=plot.see_parameters_pca]{parameters::principal_components()}}
#'  \item{\link[=plot.see_n_factors]{parameters::n_clusters()}}
#'  \item{\link[=plot.see_n_factors]{parameters::n_factors()}}
#'  \item{\link[=plot.see_parameters_simulate]{parameters::simulate_parameters()}}
#'  \item{\link[=plot.see_check_collinearity]{performance::check_collinearity()}}
#'  \item{\link[=plot.see_check_heteroscedasticity]{performance::check_heteroscedasticity()}}
#'  \item{\link[=plot.see_check_homogeneity]{performance::check_homogeneity()}}
#'  \item{\link[=plot.see_check_normality]{performance::check_normality()}}
#'  \item{\link[=plot.see_check_outliers]{performance::check_outliers()}}
#'  \item{\link[=plot.see_compare_performance]{performance::compare_performance()}}
#'  \item{\link[=plot.see_performance_roc]{performance::performance_roc()}}
#'  \item{\link[=plot.see_performance_pp_check]{performance::check_posterior_predictions()}}
#'  }
#'
#' @param x An object.
#' @param data The original data used to create this object. Can be a
#'   statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @details `data_plot()` is in most situation not needed when the purpose
#' is plotting, since most `plot()`-functions in \pkg{see} internally call
#' `data_plot()` to prepare the data for plotting.
#' \cr \cr
#' Many `plot()`-functions have a `data`-argument that is needed when
#' the data or model for plotting can't be retrieved via `data_plot()`. In
#' such cases, `plot()` gives an error and asks for providing data or models.
#' \cr \cr
#' Most `plot()`-functions work out-of-the-box, i.e. you don't need to do much
#' more than calling `plot(<object>)` (see 'Examples'). Some plot-functions
#' allow to specify arguments to modify the transparency or color of geoms,
#' these are shown in the 'Usage' section.
#'
#' @seealso [Package-Vignettes](https://easystats.github.io/see/articles/)
#'
#' @examplesIf require("rstanarm")
#' \donttest{
#' library(bayestestR)
#' library(rstanarm)
#'
#' model <<- stan_glm(
#'   Sepal.Length ~ Petal.Width * Species,
#'   data = iris,
#'   chains = 2, iter = 200, refresh = 0
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
#' model <<- stan_glm(
#'   mpg ~ wt + gear + cyl + disp,
#'   chains = 2,
#'   iter = 200,
#'   refresh = 0,
#'   data = mtcars
#' )
#' x <- equivalence_test(model)
#' plot(x)
#' }
#' @export
data_plot <- function(x, data = NULL, ...) {
  UseMethod("data_plot")
}



#' Complete figure with its attributes
#'
#' The `data_plot()` function usually stores information (such as title, axes
#' labels, etc.) as attributes, while `add_plot_attributes()` adds this
#' information to the plot.
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
#' p <- ggplot(
#'   data,
#'   aes(x = x, y = y, height = height, group = y, fill = fill)
#' ) +
#'   ggridges::geom_ridgeline_gradient()
#'
#' p
#' p + add_plot_attributes(data)
#' }
#' @export
add_plot_attributes <- function(x) {
  info <- attributes(x)$info
  out <- list(ggplot2::ylab(info$ylab), ggplot2::xlab(info$xlab))

  if (!is.null(info$legend_fill)) {
    out[[length(out) + 1]] <- ggplot2::labs(fill = info$legend_fill)
  }
  if (!is.null(info$legend_color)) {
    out[[length(out) + 1]] <- ggplot2::labs(color = info$legend_color)
  }
  if (!is.null(info$title)) {
    out[[length(out) + 1]] <- ggplot2::labs(title = info$title)
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
    dat <- tryCatch(
      {
        get(obj_name, envir = parent.frame())
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(dat)) {
      # second try, global env
      dat <- tryCatch(
        {
          get(obj_name, envir = globalenv())
        },
        error = function(e) {
          NULL
        }
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
