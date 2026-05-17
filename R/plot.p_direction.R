#' @export
data_plot.p_direction <- function(x, data = NULL, show_intercept = FALSE, ...) {
  # 1. Retrieve Data -----------------------------------------------------------
  # If no data is provided, attempt to retrieve the original model data
  # stored in the 'x' object (which is likely a p_direction result object)
  if (is.null(data)) {
    data <- .retrieve_data(x)
  }

  params <- NULL

  # 2. Format Data Based on Object Type ----------------------------------------
  # Depending on the class of the retrieved data/model, extract the posterior
  # samples and standard model parameters
  if (inherits(data, "emmGrid")) {
    insight::check_if_installed("emmeans")
    data <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(
      data,
      names = FALSE
    )))
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

  # 3. Process Multiple Parameters ---------------------------------------------
  if (ncol(data) > 1L) {
    # Store original order of parameters and subset data to those present in x
    levels_order <- rev(x$Parameter)
    data <- data[, x$Parameter, drop = FALSE]
    dataplot <- data.frame()

    # Iterate over each parameter column to compute its density
    results_list <- lapply(names(data), function(i) {
      df_dens <- .compute_densities_pd(
        data[[i]],
        name = i,
        null = attr(x, "null")
      )
      if (
        !is.null(params) && all(c("Effects", "Component") %in% colnames(params))
      ) {
        df_dens$Effects <- params$Effects[params$Parameter == i]
        df_dens$Component <- params$Component[params$Parameter == i]
      }
      df_dens
    })
    dataplot <- do.call(rbind, results_list)

    # 4. Clean up Factor Levels for Faceting -----------------------------------
    if ("Effects" %in% names(dataplot) && "Component" %in% names(dataplot)) {
      # If there's only one unique effect and component, these columns are
      # redundant for faceting, so remove them.
      if (
        length(unique(dataplot$Effects)) == 1 &&
          length(unique(dataplot$Component)) == 1
      ) {
        dataplot$Effects <- NULL
        dataplot$Component <- NULL
      } else {
        # Otherwise, ensure they are correctly leveled factors to maintain order
        dataplot$Effects <- .safe_refactor(dataplot$Effects)
        dataplot$Component <- .safe_refactor(dataplot$Component)
      }
    }
  } else {
    # 5. Process Single Parameter ----------------------------------------------
    levels_order <- NULL
    dataplot <- .compute_densities_pd(
      data[, 1],
      name = "Posterior",
      null = attr(x, "null")
    )
  }

  # 6. Calculate Probability Proportions for Coloring --------------------------
  # Calculate how many points fall on either side of the null (direction)
  # Count total per parameter/fill combination
  dataplot$n <- stats::ave(
    rep(1, nrow(dataplot)),
    dataplot$y,
    dataplot$fill,
    FUN = sum
  )
  # Total per parameter
  total_n <- stats::ave(rep(1, nrow(dataplot)), dataplot$y, FUN = sum)
  # Proportion
  dataplot$prop <- dataplot$n / total_n
  # Determine labels
  dataplot$fill2 <- ifelse(
    dataplot$prop >= 0.5,
    "Most probable",
    "Less probable"
  )
  # Drop the intermediate calculation columns
  dataplot <- dataplot[, which(!names(dataplot) %in% c("n", "prop"))]

  # 7. Final Formatting --------------------------------------------------------
  # Apply original parameter ordering
  if (!is.null(levels_order)) {
    dataplot$y <- factor(dataplot$y, levels = levels_order)
  }

  groups <- unique(dataplot$y)

  # Remove intercept if requested
  if (!show_intercept) {
    dataplot <- .remove_intercept(dataplot, column = "y", show_intercept)
    groups <- unique(setdiff(groups, .intercept_names))
  }

  # Handle y-axis labels based on remaining parameters
  if (length(groups) == 1) {
    ylab <- groups
    dataplot$y <- 0
  } else {
    ylab <- "Parameters"
  }

  dataplot <- .fix_facet_names(dataplot)

  # Attach metadata for the plot function
  attr(dataplot, "info") <- list(
    xlab = "Possible parameter values",
    ylab = ylab,
    legend_fill = "Effect direction",
    title = "Probability of Direction"
  )

  class(dataplot) <- c("data_plot", "see_p_direction", class(dataplot))
  dataplot
}


#' @keywords internal
.safe_refactor <- function(col) {
  if (is.factor(col)) {
    factor(col, levels = sort(levels(col)))
  } else {
    factor(col, levels = unique(col))
  }
}


#' @keywords internal
.compute_densities_pd <- function(x, name = "Y", null = 0) {
  # Estimate Kernel Density
  out <- .as.data.frame_density(
    stats::density(x)
  )

  if (is.null(null)) {
    null <- 0
  }

  # Assign whether the x-value of the density falls below or above the null line
  out$fill <- ifelse(out$x < null, "Negative", "Positive")

  # Normalize density heights to be between 0 and 1 (useful for ridgeline plots)
  out$height <- as.vector(
    (out$y - min(out$y, na.rm = TRUE)) /
      diff(range(out$y, na.rm = TRUE), na.rm = TRUE)
  )
  out$y <- name

  out
}


# Plot --------------------------------------------------------------------

#' Plot method for probability of direction
#'
#' The `plot()` method for the `bayestestR::p_direction()` function.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_estimate_density
#' @inheritParams plot.see_parameters_model
#'
#' @return A ggplot2-object.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && require("rstanarm")
#' library(rstanarm)
#' library(bayestestR)
#' set.seed(123)
#' m <<- suppressWarnings(stan_glm(Sepal.Length ~ Petal.Width * Species, data = iris, refresh = 0))
#' result <- p_direction(m)
#' plot(result)
#'
#' @export
plot.see_p_direction <- function(
  x,
  data = NULL,
  show_intercept = FALSE,
  priors = FALSE,
  alpha_priors = 0.4,
  n_columns = 1,
  ...
) {
  # Retrieve model for prior overlay calculation
  model <- .retrieve_data(x)

  # Ensure data is processed into plotting format
  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = data, show_intercept = show_intercept)
  }

  if (.has_multiple_panels(x)) {
    n_columns <- NULL
  }

  params <- unique(x$y)
  axis_labels <- .clean_parameter_names(x$y, grid = !is.null(n_columns))

  insight::check_if_installed("ggridges")

  # 1. Base Setup: Construct the ridgeline plot mapping ------------------------
  p <- ggplot2::ggplot(
    as.data.frame(x),
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      height = .data$height,
      group = .data$y,
      fill = .data$fill # Fill is tied to Negative/Positive direction
    )
  ) +
    ggridges::geom_ridgeline_gradient() +
    add_plot_attributes(x)

  # 2. Overlay Priors ----------------------------------------------------------
  if (priors) {
    p <- p +
      .add_prior_layer_ridgeline(
        model,
        parameter = params,
        show_intercept = show_intercept,
        alpha_priors = alpha_priors
      )
  }

  # Add Reference null line
  p <- p + ggplot2::geom_vline(aes(xintercept = 0), linetype = "dotted")

  # Format Y axis based on parameter count
  if (length(unique(x$y)) == 1 && is.numeric(x$y)) {
    p <- p + ggplot2::scale_y_continuous(breaks = NULL, labels = NULL)
  } else {
    p <- p + ggplot2::scale_y_discrete(labels = axis_labels)
  }

  # 3. Faceting ----------------------------------------------------------------
  if (!is.null(n_columns)) {
    # fmt: skip
    facets <- c("Effects", "Component")[c("Effects", "Component") %in% names(x)]
    if (length(facets) > 0) {
      # Dynamically build formula (e.g. "~ Effects + Component")
      fac_frml <- stats::as.formula(paste("~", paste(facets, collapse = " + ")))
      p <- p + ggplot2::facet_wrap(fac_frml, scales = "free", ncol = n_columns)
    }
  }

  # Apply customized continuous color scale
  p + scale_fill_flat(reverse = TRUE)
}
