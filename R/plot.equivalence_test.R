#' Plot method for (conditional) equivalence testing
#'
#' The `plot()` method for the `bayestestR::equivalence_test()` function.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_parameters_model
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(effectsize)
#' m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
#' result <- eta_squared(m)
#' plot(result)
#' @export
plot.see_equivalence_test <- function(x,
                                      rope_color = "#0171D3",
                                      rope_alpha = 0.2,
                                      show_intercept = FALSE,
                                      n_columns = 1,
                                      ...) {
  model_name <- attr(x, "object_name", exact = TRUE)

  if (is.null(model_name)) {
    warning("plot() only works for equivalence_test() with model-objects.", call. = FALSE)
    return(x)
  }


  # retrieve model
  model <- tryCatch(
    {
      get(model_name, envir = parent.frame())
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(model)) {
    warning(sprintf("Can't find object '%s'.", model_name), call. = FALSE)
    return(x)
  }

  if (inherits(model, "emmGrid")) {
    insight::check_if_installed("emmeans")
  }

  # if we have intercept-only models, keep at least the intercept
  intercepts <- which(.in_intercepts(x$Parameter))
  if (length(intercepts) && nrow(x) > length(intercepts) && !show_intercept) {
    x <- x[-intercepts, ]
  }

  cp <- insight::clean_parameters(model)
  intercepts <- which(.in_intercepts(cp$Parameter))
  if (length(intercepts) && nrow(x) > length(intercepts) && !show_intercept) {
    cp <- cp[-intercepts, ]
  }

  .rope <- c(x$ROPE_low[1], x$ROPE_high[1])

  # split for multiple CIs
  tests <- split(x, x$CI)

  result <- lapply(tests, function(i) {
    if (inherits(model, "emmGrid")) {
      tmp <- as.data.frame(as.matrix(emmeans::as.mcmc.emmGrid(model, names = FALSE)))[, i$Parameter, drop = FALSE]
    } else if (inherits(x, "equivalence_test_simulate_model")) {
      tmp <- as.data.frame(attr(x, "data"), stringsAsFactors = FALSE, optional = FALSE)[, i$Parameter, drop = FALSE]
    } else {
      tmp <- as.data.frame(model, stringsAsFactors = FALSE, optional = FALSE)[, i$Parameter, drop = FALSE]
    }

    tmp2 <- lapply(seq_len(nrow(i)), function(j) {
      p <- i$Parameter[j]
      tmp[[p]][tmp[[p]] < i$HDI_low[j]] <- NA
      tmp[[p]][tmp[[p]] > i$HDI_high[j]] <- NA
      tmp[[p]]
    })

    cnames <- colnames(tmp)
    tmp <- as.data.frame(tmp2)
    colnames(tmp) <- cnames

    tmp <- .reshape_to_long(tmp, names_to = "predictor", values_to = "estimate")
    # tmp$predictor <- as.factor(tmp$predictor)

    tmp$grp <- NA
    for (j in seq_len(nrow(i))) {
      tmp$grp[tmp$predictor == i$Parameter[j]] <- i$ROPE_Equivalence[j]
    }

    tmp$predictor <- factor(tmp$predictor)
    tmp$predictor <- factor(tmp$predictor, levels = rev(levels(tmp$predictor)))

    tmp$HDI <- sprintf("%g%% HDI", 100 * i$CI[1])

    tmp
  })

  tmp <- do.call(rbind, result)
  colnames(cp)[1] <- "predictor"
  tmp <- merge(tmp, cp, by = "predictor")
  tmp$predictor <- factor(tmp$predictor, levels = rev(unique(tmp$predictor)))

  has_multiple_panels <-
    (!"Effects" %in% names(tmp) || length(unique(tmp$Effects)) <= 1L) &&
      (!"Component" %in% names(tmp) || length(unique(tmp$Component)) <= 1L)

  # check if we have multiple panels
  if (has_multiple_panels) {
    n_columns <- NULL
  }

  # get labels
  labels <- .clean_parameter_names(tmp$predictor, grid = !is.null(n_columns))

  tmp <- .fix_facet_names(tmp)

  # check for user defined arguments

  fill.color <- c("#CD423F", "#018F77", "#FCDA3B")
  if (length(unique(tmp$HDI)) > 1L) {
    x.title <- "Highest Density Region of Posterior Samples"
  } else {
    x.title <- sprintf("%g%% Highest Density Region of Posterior Samples", 100 * x$CI[1])
  }
  legend.title <- "Decision on H0"

  fill.color <- fill.color[sort(unique(match(x$ROPE_Equivalence, c("Accepted", "Rejected", "Undecided"))))]

  add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if ("colors" %in% names(add.args)) fill.color <- eval(add.args[["colors"]])
  if ("x.title" %in% names(add.args)) x.title <- eval(add.args[["x.title"]])
  if ("legend.title" %in% names(add.args)) legend.title <- eval(add.args[["legend.title"]])
  if ("labels" %in% names(add.args)) labels <- eval(add.args[["labels"]])

  rope.line.alpha <- 1.25 * rope_alpha
  if (rope.line.alpha > 1) rope.line.alpha <- 1

  insight::check_if_installed("ggridges")

  p <- ggplot(tmp, aes(x = estimate, y = predictor, fill = grp)) +
    annotate(
      "rect",
      xmin = .rope[1],
      xmax = .rope[2],
      ymin = 0,
      ymax = Inf,
      fill = rope_color,
      alpha = (rope_alpha / 3),
      na.rm = TRUE
    ) +
    geom_vline(
      xintercept = .rope,
      linetype = "dashed",
      colour = rope_color,
      alpha = rope.line.alpha,
      na.rm = TRUE
    ) +
    geom_vline(
      xintercept = 0,
      colour = rope_color,
      linewidth = 0.8,
      alpha = rope.line.alpha,
      na.rm = TRUE
    ) +
    ggridges::geom_density_ridges2(
      rel_min_height = 0.01,
      scale = 2,
      alpha = 0.5,
      na.rm = TRUE
    ) +
    scale_fill_manual(values = fill.color) +
    labs(x = x.title, y = NULL, fill = legend.title) +
    scale_y_discrete(labels = labels) +
    theme(legend.position = "bottom")

  if (!is.null(n_columns)) {
    if ("Component" %in% names(x) && "Effects" %in% names(x)) {
      if (length(unique(tmp$HDI)) > 1L) {
        p <- p + facet_wrap(~ Effects + Component + HDI, scales = "free", ncol = n_columns)
      } else {
        p <- p + facet_wrap(~ Effects + Component, scales = "free", ncol = n_columns)
      }
    } else if ("Effects" %in% names(x)) {
      if (length(unique(tmp$HDI)) > 1L) {
        p <- p + facet_wrap(~ Effects + HDI, scales = "free", ncol = n_columns)
      } else {
        p <- p + facet_wrap(~Effects, scales = "free", ncol = n_columns)
      }
    } else if ("Component" %in% names(x)) {
      if (length(unique(tmp$HDI)) > 1L) {
        p <- p + facet_wrap(~ Component + HDI, scales = "free", ncol = n_columns)
      } else {
        p <- p + facet_wrap(~Component, scales = "free", ncol = n_columns)
      }
    }
  } else {
    if (length(unique(tmp$HDI)) > 1L) {
      p <- p + facet_wrap(~HDI, scales = "free", ncol = n_columns)
    }
  }

  p
}




# data frame method --------------------------------

#' @export
plot.see_equivalence_test_df <- function(x,
                                         rope_color = "#0171D3",
                                         rope_alpha = 0.2,
                                         data = NULL,
                                         n_columns = 1,
                                         ...) {
  if (is.null(data)) data <- .retrieve_data(x)

  if (is.null(data)) {
    insight::format_warning("plot() only works for equivalence_test() when original data frame is available.")
    return(x)
  }

  .rope <- c(x$ROPE_low[1], x$ROPE_high[1])

  # split for multiple CIs
  tests <- split(x, x$CI)

  result <- lapply(tests, function(i) {
    tmp <- data[, i$Parameter, drop = FALSE]

    tmp2 <- lapply(seq_len(nrow(i)), function(j) {
      p <- i$Parameter[j]
      tmp[[p]][tmp[[p]] < i$HDI_low[j]] <- NA
      tmp[[p]][tmp[[p]] > i$HDI_high[j]] <- NA
      tmp[[p]]
    })

    cnames <- colnames(tmp)
    tmp <- as.data.frame(tmp2)
    colnames(tmp) <- cnames

    tmp <- .reshape_to_long(tmp, names_to = "predictor", values_to = "estimate")

    tmp$grp <- NA
    for (j in seq_len(nrow(i))) {
      tmp$grp[tmp$predictor == i$Parameter[j]] <- i$ROPE_Equivalence[j]
    }

    tmp$predictor <- factor(tmp$predictor)
    tmp$predictor <- factor(tmp$predictor, levels = rev(levels(tmp$predictor)))

    tmp$HDI <- sprintf("%i%% HDI", i$CI[1])

    tmp
  })

  tmp <- do.call(rbind, result)
  tmp$predictor <- factor(tmp$predictor, levels = rev(unique(tmp$predictor)))

  # get labels
  labels <- .clean_parameter_names(tmp$predictor, grid = !is.null(n_columns))

  # check for user defined arguments

  fill.color <- c("#CD423F", "#018F77", "#FCDA3B")
  if (length(unique(tmp$HDI)) > 1L) {
    x.title <- "Highest Density Region of Posterior Samples"
  } else {
    x.title <- sprintf("%i%% Highest Density Region of Posterior Samples", x$CI[1])
  }
  legend.title <- "Decision on H0"

  fill.color <- fill.color[sort(unique(match(x$ROPE_Equivalence, c("Accepted", "Rejected", "Undecided"))))]

  add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if ("colors" %in% names(add.args)) fill.color <- eval(add.args[["colors"]])
  if ("x.title" %in% names(add.args)) x.title <- eval(add.args[["x.title"]])
  if ("legend.title" %in% names(add.args)) legend.title <- eval(add.args[["legend.title"]])
  if ("labels" %in% names(add.args)) labels <- eval(add.args[["labels"]])

  rope.line.alpha <- 1.25 * rope_alpha

  if (rope.line.alpha > 1) rope.line.alpha <- 1

  insight::check_if_installed("ggridges")

  p <- ggplot(tmp, aes(x = estimate, y = predictor, fill = grp)) +
    annotate(
      "rect",
      xmin = .rope[1],
      xmax = .rope[2],
      ymin = 0,
      ymax = Inf,
      fill = rope_color,
      alpha = (rope_alpha / 3)
    ) +
    geom_vline(
      xintercept = .rope,
      linetype = "dashed",
      colour = rope_color,
      alpha = rope.line.alpha,
      na.rm = TRUE
    ) +
    geom_vline(
      xintercept = 0,
      colour = rope_color,
      linewidth = 0.8,
      alpha = rope.line.alpha,
      na.rm = TRUE
    ) +
    ggridges::geom_density_ridges2(
      rel_min_height = 0.01,
      scale = 2,
      alpha = 0.5,
      na.rm = TRUE
    ) +
    scale_fill_manual(values = fill.color) +
    labs(x = x.title, y = NULL, fill = legend.title) +
    scale_y_discrete(labels = labels) +
    theme(legend.position = "bottom")

  if (length(unique(tmp$HDI)) > 1L) {
    p <- p + facet_wrap(~HDI, scales = "free", ncol = n_columns)
  }

  p
}



# freq models method --------------------------------

#' @rdname plot.see_equivalence_test
#' @export
plot.see_equivalence_test_lm <- function(x,
                                         size_point = 0.7,
                                         rope_color = "#0171D3",
                                         rope_alpha = 0.2,
                                         show_intercept = FALSE,
                                         n_columns = 1,
                                         ...) {
  model_name <- attr(x, "object_name", exact = TRUE)

  if (is.null(model_name)) {
    insight::format_warning("plot() only works for equivalence_test() with model-objects.")
    return(x)
  }


  # retrieve model
  model <- tryCatch(
    {
      get(model_name, envir = parent.frame())
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(model)) {
    insight::format_warning(sprintf("Can't find object '%s'.", model_name))
    return(x)
  }

  if (!"Estimate" %in% colnames(x)) {
    params <- insight::get_parameters(model, effects = "fixed", component = "conditional")
    x <- merge(x, params, sort = FALSE)
  }

  x$Parameter <- factor(x$Parameter, levels = rev(unique(x$Parameter)))

  if ("Group" %in% colnames(x)) {
    x$Group <- factor(x$Group, levels = rev(unique(x$Group)))
  }

  # if we have intercept-only models, keep at least the intercept
  intercepts <- which(.in_intercepts(x$Parameter))
  if (length(intercepts) && nrow(x) > length(intercepts) && !show_intercept) {
    x <- x[-intercepts, ]
  }

  .rope <- c(x$ROPE_low[1], x$ROPE_high[1])

  # check for user defined arguments

  fill.color <- c("#CD423F", "#018F77", "#FCDA3B")
  legend.title <- "Decision on H0"
  x.title <- NULL

  fill.color <- fill.color[sort(unique(match(x$ROPE_Equivalence, c("Accepted", "Rejected", "Undecided"))))]

  add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if ("colors" %in% names(add.args)) fill.color <- eval(add.args[["colors"]])
  if ("x.title" %in% names(add.args)) x.title <- eval(add.args[["x.title"]])
  if ("legend.title" %in% names(add.args)) legend.title <- eval(add.args[["legend.title"]])
  if ("labels" %in% names(add.args)) labels <- eval(add.args[["labels"]])

  rope.line.alpha <- 1.25 * rope_alpha
  if (rope.line.alpha > 1) rope.line.alpha <- 1

  p <- ggplot(
    x,
    aes(
      y = Parameter,
      x = Estimate,
      xmin = CI_low,
      xmax = CI_high,
      colour = ROPE_Equivalence
    )
  ) +
    annotate(
      "rect",
      xmin = .rope[1],
      xmax = .rope[2],
      ymin = 0,
      ymax = Inf,
      fill = rope_color,
      alpha = (rope_alpha / 3)
    ) +
    geom_vline(
      xintercept = .rope,
      linetype = "dashed",
      colour = rope_color,
      linewidth = 0.8,
      alpha = rope.line.alpha,
      na.rm = TRUE
    ) +
    geom_vline(
      xintercept = 0,
      colour = rope_color,
      linewidth = 0.8,
      alpha = rope.line.alpha,
      na.rm = TRUE
    ) +
    geom_pointrange(
      size = size_point,
      na.rm = TRUE
    ) +
    scale_colour_manual(values = fill.color) +
    labs(y = x.title, x = NULL, colour = legend.title) +
    theme(legend.position = "bottom") +
    scale_y_discrete()

  if ("Group" %in% colnames(x)) {
    p <- p + facet_wrap(~Group, scales = "free", ncol = n_columns)
  }

  p
}
