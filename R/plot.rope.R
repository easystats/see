#' @export
data_plot.rope <- function(x, data = NULL, show_intercept = FALSE, ...) {
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  params <- NULL

  if (inherits(data, "emmGrid")) {
    insight::check_if_installed("emmeans")
    data <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(data, names = FALSE)))
  } else if (inherits(data, c("stanreg", "brmsfit"))) {
    params <- insight::clean_parameters(data)
    data <- as.data.frame(data, optional = FALSE)
  } else if (inherits(data, "BFBayesFactor")) {
    data <- insight::get_parameters(data)
  } else if (inherits(data, "MCMCglmm")) {
    params <- insight::clean_parameters(data)
    nF <- data$Fixed$nfl
    data <- as.data.frame(data$Sol[, 1:nF, drop = FALSE])
  } else {
    data <- as.data.frame(data)
  }

  # Recontruct hdi
  hdi <- attributes(x)$HDI_area

  if (!is.data.frame(hdi)) {
    for (i in names(hdi)) {
      hdi[[i]]$Parameter <- i
    }
    hdi <- do.call("rbind", hdi)
  }

  # Extract data HDI
  dataplot <- .data_plot_hdi(hdi, data, parms = params, show_intercept = show_intercept)
  rope_range <- unique(c(x$ROPE_low, x$ROPE_high))
  if (length(rope_range) != 2) {
    stop("Only one ROPE range accepted.", call. = FALSE)
  }

  groups <- unique(dataplot$y)
  if (!show_intercept) {
    dataplot <- .remove_intercept(dataplot, column = "y", show_intercept = show_intercept)
    groups <- unique(setdiff(groups, .intercept_names))
  }

  if (length(groups) == 1) {
    dataplot$y <- 0
  }

  dataplot <- .fix_facet_names(dataplot)

  dataplot$xmin <- rope_range[1]
  dataplot$xmax <- rope_range[2]
  dataplot$color <- ifelse(dataplot$x >= dataplot$xmin & dataplot$x <= dataplot$xmax, "Negligible", "Significant")
  attributes(dataplot)$info$rope_range <- rope_range
  attributes(dataplot)$info$title <- "Region of Practical Equivalence (ROPE)"

  class(dataplot) <- c("data_plot", "see_rope", "data.frame")
  dataplot
}


# Plot --------------------------------------------------------------------

#' Plot method for Region of Practical Equivalence
#'
#' The `plot()` method for the `bayestestR::rope()`.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_parameters_model
#'
#' @return A ggplot2-object.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")
#' library(rstanarm)
#' library(bayestestR)
#' set.seed(123)
#' m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
#' result <- rope(m)
#' result
#' plot(result)
#'
#' @export
plot.see_rope <- function(x,
                          data = NULL,
                          alpha_rope = 0.5,
                          color_rope = "cadetblue",
                          show_intercept = FALSE,
                          n_columns = 1,
                          ...) {
  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = data, show_intercept = show_intercept)
  }

  if (.has_multiple_panels(x)) {
    n_columns <- NULL
  }

  # get labels
  labels <- .clean_parameter_names(x$y, grid = !is.null(n_columns))

  insight::check_if_installed("ggridges")

  p <- ggplot(
    as.data.frame(x),
    aes(
      x = .data$x,
      y = .data$y,
      height = .data$height,
      group = .data$y,
      fill = .data$fill
    )
  ) +
    ggridges::geom_ridgeline_gradient() +
    annotate(
      "rect",
      xmin = attributes(x)$info$rope_range[1],
      xmax = attributes(x)$info$rope_range[2],
      ymin = 0,
      ymax = Inf,
      fill = color_rope,
      alpha = alpha_rope
    ) +
    add_plot_attributes(x)

  if (length(unique(x$y)) == 1 && is.numeric(x$y)) {
    p <- p + scale_y_continuous(breaks = NULL, labels = NULL)
  } else {
    p <- p + scale_y_discrete(labels = labels)
  }

  if (!is.null(n_columns)) {
    if ("Component" %in% names(x) && "Effects" %in% names(x)) {
      p <- p + facet_wrap(~ Effects + Component, scales = "free", ncol = n_columns)
    } else if ("Effects" %in% names(x)) {
      p <- p + facet_wrap(~Effects, scales = "free", ncol = n_columns)
    } else if ("Component" %in% names(x)) {
      p <- p + facet_wrap(~Component, scales = "free", ncol = n_columns)
    }
  }

  p
}
