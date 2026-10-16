#' @title Prepare objects for plotting or plot objects
#' @name data_plot
#'
#' @description `data_plot()` extracts and transforms an object for plotting,
#'   while `plot()` visualizes results of functions from different packages in
#'   [easystats-project](https://github.com/easystats). See the documentation
#'   for your object's class:
#'  - \link[=plot.see_bayesfactor_models]{bayestestR::bayesfactor_models()}
#'  - \link[=plot.see_bayesfactor_parameters]{bayestestR::bayesfactor_parameters()}
#'  - \link[=plot.see_equivalence_test]{bayestestR::equivalence_test()}
#'  - \link[=plot.see_estimate_density]{bayestestR::estimate_density()}
#'  - \link[=plot.see_hdi]{bayestestR::hdi()}
#'  - \link[=plot.see_p_direction]{bayestestR::p_direction()}
#'  - \link[=plot.see_p_significance]{bayestestR::p_significance()}
#'  - \link[=plot.see_si]{bayestestR::si()}
#'  - \link[=plot.see_effectsize_table]{effectsize::effectsize()}
#'  - \link[=plot.see_estimate_contrasts]{modelbased::estimate_contrasts()}
#'  - \link[=plot.see_compare_parameters]{parameters::compare_parameters()}
#'  - \link[=plot.see_parameters_distribution]{parameters::describe_distribution()}
#'  - \link[=plot.see_parameters_model]{parameters::model_parameters()}
#'  - \link[=plot.see_parameters_pca]{parameters::principal_components()}
#'  - \link[=plot.see_n_factors]{parameters::n_clusters()}
#'  - \link[=plot.see_n_factors]{parameters::n_factors()}
#'  - \link[=plot.see_parameters_simulate]{parameters::simulate_parameters()}
#'  - \link[=plot.see_check_collinearity]{performance::check_collinearity()}
#'  - \link[=plot.see_check_heteroscedasticity]{performance::check_heteroscedasticity()}
#'  - \link[=plot.see_check_homogeneity]{performance::check_homogeneity()}
#'  - \link[=plot.see_check_normality]{performance::check_normality()}
#'  - \link[=plot.see_check_outliers]{performance::check_outliers()}
#'  - \link[=plot.see_compare_performance]{performance::compare_performance()}
#'  - \link[=plot.see_performance_roc]{performance::performance_roc()}
#'  - \link[=plot.see_performance_pp_check]{performance::check_predictions()}
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
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")
#' library(bayestestR)
#' library(rstanarm)
#'
#' model <<- suppressWarnings(stan_glm(
#'   Sepal.Length ~ Petal.Width * Species,
#'   data = iris,
#'   chains = 2, iter = 200, refresh = 0
#' ))
#'
#' x <- rope(model, verbose = FALSE)
#' plot(x)
#'
#' x <- hdi(model)
#' plot(x) + theme_modern()
#'
#' x <- p_direction(model, verbose = FALSE)
#' plot(x)
#'
#' model <<- suppressWarnings(stan_glm(
#'   mpg ~ wt + gear + cyl + disp,
#'   chains = 2,
#'   iter = 200,
#'   refresh = 0,
#'   data = mtcars
#' ))
#' x <- equivalence_test(model, verbose = FALSE)
#' plot(x)
#' @export
data_plot <- function(x, ...) {
  UseMethod("data_plot")
}


#' Complete figure with its attributes
#'
#' The `data_plot()` function usually stores information (such as title, axes
#' labels, etc.) as attributes, while `add_plot_attributes()` adds this
#' information to the plot.
#'
#' @inheritParams data_plot
#' @examplesIf require("rstanarm", quietly = TRUE)
#' \donttest{
#' library(rstanarm)
#' library(bayestestR)
#' library(see)
#' library(ggplot2)
#'
#' model <- suppressWarnings(stan_glm(
#'   Sepal.Length ~ Petal.Width + Species + Sepal.Width,
#'   data = iris,
#'   chains = 2, iter = 200, refresh = 0
#' ))
#'
#' result <- bayestestR::hdi(model, ci = c(0.5, 0.75, 0.9, 0.95))
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
    out[[length(out) + 1L]] <- ggplot2::labs(fill = info$legend_fill)
  }
  if (!is.null(info$legend_color)) {
    out[[length(out) + 1L]] <- ggplot2::labs(color = info$legend_color)
  }
  if (!is.null(info$title)) {
    out[[length(out) + 1L]] <- ggplot2::labs(title = info$title)
  }
  if (!is.null(info$subtitle)) {
    out[[length(out) + 1L]] <- ggplot2::labs(subtitle = info$subtitle)
  }

  out
}


#' @keywords internal
.retrieve_data <- function(x) {
  # retrieve model
  obj_name <- attr(x, "object_name", exact = TRUE)
  dat <- NULL

  # for simulated residuals, we save all necessary information in the object
  if (inherits(x, "performance_simres")) {
    return(x$fittedModel)
  }

  if (!is.null(obj_name)) {
    # first try, the frames of the user's code on the call stack, so that an
    # object made inside a function is found, and wins over a global object
    # of the same name
    dat <- .dynGet(obj_name, ifnotfound = NULL)

    if (is.null(dat)) {
      # second try, global env and search path
      dat <- tryCatch(
        get(obj_name, envir = globalenv()),
        error = function(e) NULL
      )
    }
  }

  if (is.null(dat)) {
    dat <- attr(x, "data", exact = TRUE)
  }

  if (is.null(dat) && !is.null(obj_name)) {
    # for a list, such as in `describe_distribution(list(...))`, the stored
    # name is the deparsed list itself
    dat <- .eval_constant_list(obj_name)
  }

  if (is.null(dat)) {
    insight::format_error(
      "Failed at retrieving data :( Please provide original model or data through the `data` argument"
    )
  }

  dat
}

#' @keywords internal
.dynGet <- function(
  x,
  ifnotfound = stop(
    gettextf("%s not found", sQuote(x)),
    domain = NA,
    call. = FALSE
  ),
  minframe = 1L,
  inherits = FALSE
) {
  # the stored object name is already a string; deparsing it would add quotes
  if (!is.character(x)) {
    x <- insight::safe_deparse(x)
  }
  n <- sys.nframe()
  myObj <- structure(list(.b = as.raw(7)), foo = 47L)
  while (n > minframe) {
    n <- n - 1L
    # skip calls of package functions (see, the generics, other packages'
    # methods), whose local variables such as `x` or `data` are not the
    # user's objects
    if (.is_package_frame(n)) {
      next
    }
    env <- sys.frame(n)
    r <- get0(x, envir = env, inherits = inherits, ifnotfound = myObj)
    if (!identical(r, myObj)) {
      return(r)
    }
  }
  ifnotfound
}

#' @keywords internal
.eval_constant_list <- function(obj_name) {
  expr <- tryCatch(
    str2lang(paste(obj_name, collapse = "")),
    error = function(e) NULL
  )
  if (
    !is.call(expr) ||
      !identical(expr[[1]], as.name("list")) ||
      !.is_constant_list(expr)
  ) {
    return(NULL)
  }
  # evaluate with nothing but list(), c() and the minus of negative numbers
  allowed <- list(list = base::list, c = base::c, `-` = base::`-`)
  eval(expr, envir = list2env(allowed, parent = emptyenv()))
}

#' @keywords internal
.is_constant_list <- function(expr) {
  all(vapply(
    as.list(expr)[-1],
    function(e) {
      if (is.call(e) && identical(e[[1]], as.name("c"))) {
        .is_constant_vector(e)
      } else {
        .is_constant(e)
      }
    },
    logical(1)
  ))
}

#' @keywords internal
.is_constant_vector <- function(expr) {
  all(vapply(as.list(expr)[-1], .is_constant, logical(1)))
}

#' @keywords internal
.is_constant <- function(e) {
  if (is.call(e)) {
    # a negative number, such as -1.5 or -Inf
    return(
      identical(e[[1]], as.name("-")) &&
        length(e) == 2L &&
        is.numeric(e[[2]]) &&
        length(e[[2]]) == 1L
    )
  }
  # Inf, NaN and NA_real_ are parsed as constants, not as names
  (is.numeric(e) || is.character(e) || is.logical(e)) && length(e) == 1L
}

#' @keywords internal
.is_package_frame <- function(n) {
  # the frame of a package function encloses the package namespace; the frame
  # of user code, or of eval() and local() in user code, encloses the global
  # environment
  isNamespace(topenv(sys.frame(n)))
}
