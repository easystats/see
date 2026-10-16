#' @export
data_plot.estimate_contrasts <- function(x, data = NULL, ...) {
  .data_plot_estimate_contrasts(x, data)
}


#' @keywords internal
.data_plot_estimate_contrasts <- function(x, means = NULL, ...) {
  if (is.null(means)) {
    insight::format_error(
      "Please provide the estimated means data obtained via `estimate_means()`."
    )
  }

  # the contrasted variables are on the x axis; if the contrasts have no
  # `contrast` attribute, fall back to the first column of the means
  x_name <- .clean_target_names(attr(x, "contrast"))
  if (is.null(x_name)) {
    x_name <- names(means)[1]
  }

  if (!all(c("Level1", "Level2") %in% names(x))) {
    insight::format_error(
      "Lighthouse plots need pairwise contrasts with the columns `Level1` and `Level2`."
    )
  }
  if (!all(c("CI_low", "CI_high") %in% names(x))) {
    insight::format_error(
      "Lighthouse plots need contrasts with the columns `CI_low` and `CI_high`."
    )
  }
  # an unknown transformation comes first: modelbased then sets
  # `transform = FALSE` on the means
  geometry <- .lighthouse_geometry(x)
  .check_same_scale(x, means)

  # each contrast is drawn from the means of its own `by` level
  by_vars <- .clean_target_names(attr(x, "by"))

  # for ordinal and multinomial models, the means have one row per response
  # category, and each contrast compares two level-by-category cells; only
  # contrasts within one category are drawn, with one panel per category
  response_categories <- all(c("Response1", "Response2") %in% names(x))

  # the means call to suggest keeps the target values of the contrasts
  means_terms <- c(attr(x, "contrast") %||% x_name, attr(x, "by"))
  means_call <- paste0(
    "`estimate_means(by = c(",
    toString(vapply(means_terms, deparse, character(1), USE.NAMES = FALSE)),
    "))`"
  )

  missing_columns <- setdiff(c(x_name, by_vars), names(means))
  if (length(missing_columns)) {
    insight::format_error(
      paste0(
        "The estimated means lack the column(s) ",
        toString(missing_columns),
        " that the contrasts need. Please obtain the means with ",
        means_call,
        "."
      )
    )
  }

  y_name <- .estimate_column(means)

  contrast_data <- as.data.frame(x)
  means <- as.data.frame(means)
  if (response_categories) {
    if (!"Response" %in% names(means)) {
      insight::format_error(
        paste(
          "The contrasts compare response categories, but the estimated",
          "means lack the column `Response`. Please obtain the means with",
          "`estimate_means()` for the same model."
        )
      )
    }
    within <- as.character(contrast_data$Response1) ==
      as.character(contrast_data$Response2)
    contrast_data <- contrast_data[within, , drop = FALSE]
    rownames(contrast_data) <- NULL
    contrast_data$Response <- contrast_data$Response1
    contrast_data$Response1 <- contrast_data$Response2 <- NULL
    by_vars <- c("Response", by_vars)
  }

  # `by` columns named like columns of the plot data are renamed
  by_columns <- by_vars
  collides <- by_vars %in%
    c("x", "y", "ymin", "ymax", "group", "Mean1", "Mean2")
  if (any(collides)) {
    by_columns[collides] <- paste0(".by_", by_vars[collides])
    names(contrast_data)[match(
      by_vars[collides],
      names(contrast_data)
    )] <- by_columns[collides]
    names(means)[match(by_vars[collides], names(means))] <- by_columns[collides]
  }

  # panels only for the `by` levels that the contrasts have
  if (length(by_columns)) {
    keep <- .by_key(means, by_columns) %in% .by_key(contrast_data, by_columns)
    means <- means[keep, , drop = FALSE]
  }

  dataplot <- .data_contrasts_and_means(
    contrast_data,
    means,
    x_name = x_name,
    y_name = y_name,
    by = by_columns,
    means_call = means_call,
    geometry = geometry
  )

  attr(dataplot, "info") <- list(
    xlab = toString(x_name),
    ylab = y_name,
    title = paste0("Estimated ", sub("y$", "ie", y_name), "s and Contrasts"),
    x_name = x_name,
    by = by_columns
  )

  class(dataplot) <- c("data_plot", "see_estimate_contrasts", class(dataplot))
  dataplot
}


# the column of the estimates in the means: the one that modelbased names in
# `coef_name`, else the first of the usual names
#' @keywords internal
.estimate_column <- function(means) {
  coef_name <- attr(means, "coef_name", exact = TRUE)
  if (
    is.character(coef_name) &&
      length(coef_name) == 1L &&
      coef_name %in% names(means)
  ) {
    return(coef_name)
  }

  candidates <- c(
    "Median",
    "Mean",
    "MAP",
    "Coefficient",
    "Probability",
    "Proportion",
    "Rate"
  )
  y_name <- candidates[candidates %in% names(means)][1]
  if (is.na(y_name)) {
    insight::format_error(
      paste0(
        "The estimated means have no column of estimates. Lighthouse plots ",
        "look for the column that the `coef_name` attribute of the means ",
        "names, and then for the columns ",
        toString(candidates),
        "."
      )
    )
  }
  y_name
}


# lighthouses span the estimate of Level1 minus the interval of a difference,
# or divided by the interval of a ratio
#' @keywords internal
.lighthouse_geometry <- function(contrasts) {
  back_transformed <- isTRUE(attr(contrasts, "transform", exact = TRUE))
  # Bayesian ratio contrasts have a `Median` column, not `Ratio`, so the
  # `comparison` formula names the ratio
  comparison <- attr(contrasts, "comparison", exact = TRUE)
  ratio_comparison <- inherits(comparison, "formula") &&
    length(comparison) == 3L &&
    identical(comparison[[2]], as.name("ratio"))
  if ("Ratio" %in% names(contrasts) || ratio_comparison) {
    # e.g. for a log response, the back-transformed ratio is
    # exp(log(m1) / log(m2)), not a ratio of the back-transformed means;
    # without a transformation, back-transforming changes nothing
    if (
      back_transformed &&
        !identical(.response_transformation(contrasts), "identity")
    ) {
      insight::format_error(
        paste(
          "Lighthouse plots do not draw ratio contrasts estimated with",
          "`transform = TRUE`, because for a transformed response they are",
          "not ratios of the back-transformed means. Please obtain the",
          "contrasts and means without `transform`."
        )
      )
    }
    return("ratio")
  }
  # emmeans back-transforms the means and then takes their differences
  if (
    !back_transformed ||
      identical(attr(contrasts, "backend", exact = TRUE), "emmeans")
  ) {
    return("difference")
  }

  # back-transformed contrasts of a log response are ratios of the
  # back-transformed means
  transformation <- .response_transformation(contrasts)
  if (is.null(transformation)) {
    insight::format_error(
      paste(
        "Lighthouse plots cannot draw contrasts estimated with",
        "`transform = TRUE`, because the transformation of the response is",
        "unknown. Please obtain the contrasts and means without `transform`."
      )
    )
  }
  if (identical(transformation, "identity")) {
    return("difference")
  }
  if (.is_log_transformation(transformation)) {
    return("ratio")
  }
  insight::format_error(
    paste0(
      "Lighthouse plots cannot draw contrasts back-transformed from the ",
      "transformation `",
      transformation,
      "` of the response. Please obtain the contrasts and means without ",
      "`transform`, or fit the model to a response transformed with log(), ",
      "log2(), or log10()."
    )
  )
}


# the transformation of the response of the model that the contrasts carry,
# or NULL if it is unknown
#' @keywords internal
.response_transformation <- function(contrasts) {
  model <- attr(contrasts, "model", exact = TRUE)
  transformation <- if (!is.null(model)) {
    tryCatch(insight::find_transformation(model), error = function(e) NULL)
  }
  if (!is.character(transformation) || length(transformation) != 1L) {
    return(NULL)
  }
  transformation
}


#' @keywords internal
.is_log_transformation <- function(x) {
  if (!is.character(x) || length(x) != 1L) {
    return(FALSE)
  }
  # tolerate spaces, as in "log(x, base = 2)"
  x <- gsub("\\s", "", x)
  x %in% c("log", "log2", "log10") || startsWith(x, "log(x,base=")
}


# the contrasts and means must be on one scale
#' @keywords internal
.check_same_scale <- function(contrasts, means) {
  .check_same_predict(contrasts, means)
  .check_same_transform(contrasts, means)
}


# values that modelbased gives for the response scale count as the same;
# emmeans records the link scale as "none"; the link scale is the response
# scale only with an identity link, an untransformed response, and no
# zero-inflation or hurdle part
#' @keywords internal
.check_same_predict <- function(contrasts, means) {
  predict_contrasts <- attr(contrasts, "predict", exact = TRUE)
  predict_means <- attr(means, "predict", exact = TRUE)
  if (
    !is.character(predict_contrasts) ||
      length(predict_contrasts) != 1L ||
      !is.character(predict_means) ||
      length(predict_means) != 1L
  ) {
    return(invisible(NULL))
  }

  model <- attr(contrasts, "model", exact = TRUE)
  info <- if (!is.null(model)) {
    tryCatch(
      insight::model_info(model, verbose = FALSE),
      error = function(e) NULL
    )
  }
  identity_link <- identical(info$link_function, "identity") &&
    !isTRUE(info$is_zero_inflated) &&
    !isTRUE(info$is_hurdle) &&
    identical(.response_transformation(contrasts), "identity")
  predict_scale <- function(predict) {
    if (predict %in% c("response", "inverse_link", "invlink(link)")) {
      return("response")
    }
    if (predict == "none") {
      predict <- "link"
    }
    if (predict == "link" && identity_link) {
      return("response")
    }
    predict
  }

  if (
    !identical(predict_scale(predict_contrasts), predict_scale(predict_means))
  ) {
    insight::format_error(
      paste0(
        "The contrasts were estimated with `predict = \"",
        predict_contrasts,
        "\"` and the estimated means with `predict = \"",
        predict_means,
        "\"`. Lighthouse plots need both on the same scale. Please obtain ",
        "the contrasts and means with the same `predict` and the same ",
        "`backend`."
      )
    )
  }
}


#' @keywords internal
.check_same_transform <- function(contrasts, means) {
  transform_contrasts <- attr(contrasts, "transform", exact = TRUE)
  transform_means <- attr(means, "transform", exact = TRUE)
  if (
    is.null(transform_contrasts) ||
      is.null(transform_means) ||
      identical(transform_contrasts, transform_means, ignore.environment = TRUE)
  ) {
    return(invisible(NULL))
  }
  describe <- function(transform) {
    if (is.function(transform)) {
      "a `transform` function"
    } else {
      paste0("`transform = ", deparse(transform), "`")
    }
  }
  if (is.function(transform_contrasts) || is.function(transform_means)) {
    insight::format_error(
      paste0(
        "The contrasts were estimated with ",
        describe(transform_contrasts),
        " and the estimated means with ",
        describe(transform_means),
        ". Lighthouse plots cannot check that contrasts and means estimated ",
        "with a `transform` function are on the same scale. Please obtain ",
        "the contrasts and means without a `transform` function."
      )
    )
  }
  # without a transformation, back-transforming changes nothing
  if (identical(.response_transformation(contrasts), "identity")) {
    return(invisible(NULL))
  }
  insight::format_error(
    paste0(
      "The contrasts were estimated with `transform = ",
      deparse(transform_contrasts),
      "` and the estimated means with `transform = ",
      deparse(transform_means),
      "`. Lighthouse plots need both on the same scale. Please obtain the ",
      "contrasts and means with the same `transform`."
    )
  )
}


# modelbased keeps target values in the attributes, e.g. "wt=c(2,3)";
# the column in the data is named by the part before "="
#' @keywords internal
.clean_target_names <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  insight::trim_ws(sub("=.*$", "", x))
}


# values as text for matching the data to the means; numbers as doubles, so
# that 100000L in the data matches 1e5 in the means
#' @keywords internal
.value_key <- function(x) {
  if (is.numeric(x)) {
    as.character(as.numeric(x))
  } else {
    as.character(x)
  }
}


# three or more values whose gaps between neighbors differ by more than a
# tolerance, so that 0.1, 0.2, 0.3 count as evenly spaced
#' @keywords internal
.uneven_spacing <- function(x) {
  values <- sort(unique(as.numeric(x)))
  if (length(values) < 3L) {
    return(FALSE)
  }
  gaps <- diff(values)
  max(gaps) - min(gaps) > 1e-8 * (max(values) - min(values))
}


# one key per row for the combination of the values in `columns`
#' @keywords internal
.by_key <- function(data, columns) {
  do.call(
    paste,
    c(lapply(columns, function(i) .value_key(data[[i]])), sep = "\r")
  )
}


#' @keywords internal
.data_contrasts_and_means <- function(
  contrasts,
  means,
  x_name,
  y_name,
  by = NULL,
  means_call = NULL,
  geometry = "difference"
) {
  polygons <- as.data.frame(contrasts)
  polygons$group <- seq_len(nrow(polygons))
  polygons$Level1 <- as.character(polygons$Level1)
  polygons$Level2 <- as.character(polygons$Level2)

  # the estimate divided by an interval that includes 0 has no bounds
  includes_0 <- geometry == "ratio" &
    !is.na(polygons$CI_low) &
    !is.na(polygons$CI_high) &
    polygons$CI_low <= 0 &
    polygons$CI_high >= 0
  if (any(includes_0)) {
    insight::format_warning(
      paste0(
        sum(includes_0),
        " of ",
        nrow(contrasts),
        " ratio contrasts are not drawn, because their confidence interval ",
        "includes 0."
      )
    )
  }
  polygons <- polygons[!includes_0, ]

  data_means <- as.data.frame(means)

  # a joint contrast compares combinations of levels, which modelbased labels
  # by pasting the levels of the contrasted variables with ", "
  x_labels <- do.call(
    paste,
    c(lapply(x_name, function(i) as.character(data_means[[i]])), sep = ", ")
  )

  # keep the level order of the contrasted variables on the x axis, with the
  # first variable varying slowest
  level_sets <- lapply(x_name, function(i) {
    values <- data_means[[i]]
    if (is.factor(values)) {
      levels(values)[levels(values) %in% values]
    } else {
      unique(as.character(values))
    }
  })
  level_grid <- expand.grid(rev(level_sets), stringsAsFactors = FALSE)
  x_levels <- do.call(paste, c(rev(as.list(level_grid)), sep = ", "))
  x_levels <- x_levels[x_levels %in% x_labels]

  # one numeric contrast variable with unevenly spaced values keeps its
  # spacing on a continuous axis; the contrast levels are matched to the
  # means by their labels
  numeric_x <- length(x_name) == 1L &&
    is.numeric(data_means[[x_name]]) &&
    .uneven_spacing(data_means[[x_name]])
  x_position <- function(labels) {
    if (numeric_x) {
      as.numeric(data_means[[x_name]])[match(labels, x_labels)]
    } else {
      factor(labels, levels = x_levels)
    }
  }

  data_means$x <- x_position(x_labels)
  data_means$y <- data_means[[y_name]]
  data_means$Level2 <- data_means$Level1 <- x_labels
  data_means$Mean2 <- data_means$Mean1 <- data_means[[y_name]]
  data_means$ymin <- data_means$CI_low
  data_means$ymax <- data_means$CI_high

  # each contrast level must match exactly one row of the means
  if (anyDuplicated(data_means[c("Level1", by)])) {
    insight::format_error(
      paste0(
        "The estimated means have more than one row for some levels of ",
        toString(c(x_name, by)),
        ". Please obtain the means with ",
        means_call,
        ", without other variables."
      )
    )
  }

  polygons <- merge(
    polygons,
    data_means[c("Level1", by, "Mean1")],
    by = c("Level1", by)
  )
  polygons <- merge(
    polygons,
    data_means[c("Level2", by, "Mean2")],
    by = c("Level2", by)
  )

  # contrasts without a means row on both sides (e.g. "trt_vs_ctrl", or
  # means estimated at other values) are not drawn
  not_drawn <- setdiff(
    seq_len(nrow(contrasts)),
    c(polygons$group, which(includes_0))
  )
  if (length(not_drawn)) {
    insight::format_warning(
      paste0(
        length(not_drawn),
        " of ",
        nrow(contrasts),
        " contrasts are not drawn, because the estimated means have no row for their levels.",
        " Lighthouse plots draw only pairwise contrasts between ",
        if (length(x_name) > 1) "combinations of levels of " else "levels of ",
        toString(x_name),
        ", with the means estimated at the same values as the contrasts: ",
        means_call,
        "."
      )
    )
  }

  # a difference is Mean1 - Mean2 and a ratio is Mean1 / Mean2, so the
  # lighthouse at Level2 spans the values of Mean2 that the interval allows
  tip <- if (geometry == "ratio") `/` else `-`
  polygons <- rbind(
    cbind(
      polygons,
      data.frame(
        x = x_position(polygons$Level1),
        y = polygons$Mean1
      )
    ),
    cbind(
      polygons,
      data.frame(
        x = x_position(polygons$Level2),
        y = tip(polygons$Mean1, polygons$CI_low)
      )
    ),
    cbind(
      polygons,
      data.frame(
        x = x_position(polygons$Level2),
        y = tip(polygons$Mean1, polygons$CI_high)
      )
    )
  )

  list(
    geom_polygon = polygons,
    geom_pointrange = data_means
  )
}


# Plot --------------------------------------------------------------------

#' Plot method for estimating contrasts
#'
#' The `plot()` method for the `modelbased::estimate_contrasts()`
#' function.
#'
#' If you estimate the contrasts with `by`, the plot draws each contrast from
#' the estimated means of its own `by` level. The plot then has one panel per
#' level, or combination of levels, of the `by` variables. The estimated means
#' must include the contrasted variables and the `by` variables, and no other
#' variables that split their rows further, except the `Response` column of
#' ordinal and multinomial models. If the means have no row for the levels of
#' a contrast, the plot does not draw that contrast and gives a warning.
#'
#' For ordinal and multinomial models, such as `MASS::polr()` and
#' `mclogit::mblogit()`, the means have one row per response category, in the
#' column `Response`. The contrasts have the columns `Response1` and
#' `Response2`. The plot then has one panel per response category, or per
#' combination of category and `by` levels. Each panel shows the means of that
#' category and the contrasts whose `Response1` and `Response2` are both that
#' category. The plot does not draw contrasts between two categories, and it
#' gives no warning for them.
#'
#' If you contrast two or more variables jointly, for example with
#' `contrast = c("am", "cyl")`, the x axis shows their combinations of levels,
#' such as `"auto, 4"`. The first variable changes slowest along the axis.
#'
#' The plot takes the estimates from the column of the means that their
#' `coef_name` attribute names. For means without this attribute, such as a
#' plain data frame, it takes the first of the columns `Median`, `Mean`, `MAP`,
#' `Coefficient`, `Probability`, `Proportion`, and `Rate`. If the means have
#' none of these columns, the plot stops with an error.
#'
#' Each lighthouse starts at the estimated mean of `Level1` of a contrast. For
#' a difference, the lighthouse at `Level2` spans that mean minus the bounds of
#' the confidence interval. Contrasts with a `Ratio` column, from
#' `comparison = ratio ~ pairwise`, are ratios of the means, so the lighthouse
#' at `Level2` spans that mean divided by the bounds of the interval. Bayesian
#' ratio contrasts have no `Ratio` column, so the plot reads the left side of
#' their `comparison` formula, `ratio`, and draws them as ratios too. If the
#' interval of a ratio includes 0, the plot does not draw that contrast and
#' gives a warning. The contrasts must have the columns `CI_low` and
#' `CI_high`.
#'
#' Contrasts estimated with `transform = TRUE` are back-transformed to the scale
#' of the response. With the default backend, if the model response is
#' `log(y)`, `log(y, base = b)`, `log2(y)`, or `log10(y)`, these contrasts are
#' ratios of the back-transformed means, and the plot draws them as ratios. If
#' `insight::find_transformation()` reports the transformation `"identity"`,
#' as it does for an untransformed response and for `scale(y)`, the plot draws
#' them as differences. For other transformations, such as `sqrt(y)`, and for
#' transformations that `insight::find_transformation()` does not recognize,
#' the plot stops with an error. With `backend = "emmeans"`, which sets
#' `transform = TRUE` by default, the contrasts are differences of the
#' back-transformed means, and the plot draws them as differences. The plot
#' draws ratio contrasts estimated with `transform = TRUE` as ratios if
#' `insight::find_transformation()` reports the transformation `"identity"`.
#' For other transformations, it stops with an error, because for a
#' transformed response they are not ratios of the back-transformed means.
#'
#' The contrasts and the means must be on the same scale. The plot stops with
#' an error if both record a `predict` value and the two differ. The `predict`
#' values `"response"`, `"inverse_link"`, and `"invlink(link)"` count as the
#' same scale. With `backend = "emmeans"`, modelbased records the link scale as
#' `predict = "none"`, which counts as `"link"`. For a model with an identity
#' link, a response that `insight::find_transformation()` reports as
#' `"identity"`, and no zero-inflation or hurdle part, such as `lm(y ~ x)`,
#' `"link"` and `"none"` count as the response scale.
#' The plot also stops with an error if both record a `transform`
#' value and the two differ. If neither value is a `transform` function and
#' `insight::find_transformation()` reports the transformation `"identity"`,
#' the plot does not stop. Contrasts and means estimated with the same
#' `transform` function also differ, because modelbased records the function
#' on the contrasts and `transform = TRUE` on the means, so the plot stops.
#'
#' @inheritParams data_plot
#' @param data The estimated means, obtained with
#'   `modelbased::estimate_means()`.
#' @param show_data Logical. If `TRUE`, the plot draws the raw data of the model
#'   as jittered points behind the lighthouses. The raw data come from
#'   `insight::get_data()` for the model stored in `x`, not from `data`. A
#'   response that is logical or a factor with two levels is drawn as 0 and 1,
#'   with `TRUE` or the second level as 1. A response `log(y)`, `log2(y)`, or
#'   `log10(y)`, with `y` a column of the data, is drawn as the raw data of `y`
#'   if both the contrasts and the means were estimated with
#'   `transform = TRUE`. Means without the `transform` attribute, such as a
#'   plain data frame, then give an error. Ratio contrasts are drawn with the
#'   raw data. The plot stops with an error in these cases:
#'   - `show_data` is not a single `TRUE` or `FALSE`.
#'   - `x` is the output of `data_plot()`, or it does not carry its model, or
#'     the model data cannot be retrieved.
#'   - The response is a `cbind()` of successes and failures in a binomial
#'     model.
#'   - The model transforms the response, for example `log(y)`, and the
#'     contrasts and means are not back-transformed log contrasts and means as
#'     above.
#'   - The `predict` of the contrasts or of the estimated means is not one of
#'     `"prediction"`, `"response"`, `"expectation"`, `"invlink(link)"`, and
#'     `"inverse_link"`, for example `predict = "link"`. Means without this
#'     information, such as a plain data frame, are taken to be on the same
#'     scale as the contrasts.
#'   - The model data lack the response or a contrasted or `by` variable.
#'   - The estimated means do not include each value in the data of the
#'     contrasted and `by` variables, or each combination of their values. For
#'     example, a numeric `by` variable has values that the means were not
#'     estimated at.
#'
#' @details
#' **Interpreting Lighthouse Plots**
#'
#' Lighthouse plots display both estimated marginal means and the uncertainty
#' around their pairwise differences (contrasts) in a single visual:
#'
#' - **Means and Error Bars:** The points and vertical line ranges display the
#'   estimated marginal means for each level along with their individual confidence
#'   intervals.
#' - **Lighthouse Beams (Projected Uncertainty):** The shaded polygonal bands
#'   ("beams") represent the pairwise contrasts. A beam originates at the point
#'   estimate of a reference level and projects toward the comparison level.
#'   At the target level, the vertical span of the beam represents the confidence
#'   interval of the estimated difference, anchored at the reference mean.
#' - **Interpretation:** The vertical span of the projected beam at the target
#'   level corresponds to the confidence interval of the estimated difference,
#'   evaluated *relative* to the reference point estimate. Intuitively, it answers:
#'   *"If the reference level is at value Y, where do we expect the comparison
#'   level to be based on the estimated contrast?"*
#' - **Reading Significance:** Because the beam illustrates the confidence interval
#'   of the difference, zero difference corresponds to a horizontal line from the
#'   reference point. If both edges of the beam tilt in the same direction (both
#'   rising or both falling), the confidence interval does not include zero (the
#'   difference is statistically significant). If one edge rises while the other
#'   falls, the beam straddles the horizontal line, indicating that a difference
#'   of zero remains plausible.
#'
#' \if{html}{
#'   \cr
#'   \figure{lighthouse.png}{options: alt="Lighthouse plot for two studies: one with significant differences, one without."}
#'   \out{<small>}
#'     \emph{
#'       Lighthouse plots of the two studies. In black, each condition's mean and
#'       95\% CI, identical in both panels. In grey, the beam: the 95\% CI of the
#'       difference between B and A, cast from B. The dashed horizontal line,
#'       added here as a reading aid, makes it easier to see which way each edge
#'       tilts.
#'     }
#'   \out{</small>}
#'   \cr
#' }
#'
#' @return A ggplot2-object.
#'
#' @examplesIf all(insight::check_if_installed(c("marginaleffects", "Formula"), quietly = TRUE))
#' \donttest{
#' library(modelbased)
#'
#' model <- lm(Sepal.Width ~ Species, data = iris)
#' contrasts <- estimate_contrasts(model)
#' means <- estimate_means(model)
#' plot(contrasts, means)
#'
#' # with the raw data as jittered points
#' plot(contrasts, means, show_data = TRUE)
#'
#' # contrasts of `am` within each level of `cyl`, one panel per level
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' d$am <- factor(d$am, labels = c("auto", "manual"))
#' model <- lm(mpg ~ cyl * am, data = d)
#' contrasts <- estimate_contrasts(model, contrast = "am", by = "cyl")
#' means <- estimate_means(model, by = c("cyl", "am"))
#' plot(contrasts, means)
#'
#' # joint contrasts of `am` and `cyl`, on an axis of combined levels
#' contrasts <- estimate_contrasts(model, contrast = c("am", "cyl"))
#' plot(contrasts, means)
#' }
#'
#' @export
plot.see_estimate_contrasts <- function(
  x,
  data = NULL,
  show_data = FALSE,
  ...
) {
  if (!is.logical(show_data) || length(show_data) != 1L || is.na(show_data)) {
    insight::format_error("`show_data` must be `TRUE` or `FALSE`.")
  }

  contrast_data <- x
  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = data)
  } else if (show_data) {
    insight::format_error(
      paste(
        "`show_data = TRUE` needs the contrasts from `estimate_contrasts()`,",
        "because the output of `data_plot()` does not carry the model."
      )
    )
  }

  p <- ggplot2::ggplot()

  # the raw data are the first layer, behind the lighthouses; on a continuous
  # axis the jitter is a tenth of the smallest gap between the x values
  if (show_data) {
    x_values <- sort(unique(x$geom_pointrange$x))
    jitter_width <- if (is.numeric(x_values) && length(x_values) > 1L) {
      0.1 * min(diff(x_values))
    } else {
      0.1
    }
    p <- p +
      ggplot2::geom_point(
        data = .raw_data_contrasts(contrast_data, data, x),
        ggplot2::aes(x = .data$x, y = .data$y),
        position = ggplot2::position_jitter(width = jitter_width, height = 0),
        alpha = 0.3
      )
  }

  p <- p +
    ggplot2::geom_polygon(
      data = x$geom_polygon,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
      alpha = 0.1
    ) +
    ggplot2::geom_pointrange(
      data = x$geom_pointrange,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        ymax = .data$ymax,
        ymin = .data$ymin
      ),
      color = "black"
    ) +
    add_plot_attributes(x)

  # a continuous axis has its breaks at the values of the means
  if (is.numeric(x$geom_pointrange$x)) {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = sort(unique(x$geom_pointrange$x))
      )
  }

  # one panel per level, or combination of levels, of the `by` variables
  by_columns <- attr(x, "info")$by
  if (length(by_columns)) {
    p <- p + ggplot2::facet_wrap(by_columns)
  }

  p
}


# the model's data, with x and y as the lighthouse plot `dataplot` draws them
#' @keywords internal
.raw_data_contrasts <- function(contrasts, means, dataplot) {
  if (all(c("Response1", "Response2") %in% names(contrasts))) {
    insight::format_error(
      paste(
        "`show_data = TRUE` cannot draw the raw data of ordinal or",
        "multinomial responses, because the plot shows the probability of",
        "each response category, not values of the response."
      )
    )
  }

  # exact lookups: "model" would also match the "model_info" attribute
  model <- attr(contrasts, "model", exact = TRUE)
  if (is.null(model)) {
    insight::format_error(
      paste(
        "`show_data = TRUE` needs the model, but the contrasts do not carry it.",
        "Please obtain the contrasts with `estimate_contrasts()`."
      )
    )
  }

  info <- attr(dataplot, "info")
  x_name <- info$x_name
  by_vars <- .clean_target_names(attr(contrasts, "by", exact = TRUE))
  response <- attr(contrasts, "response", exact = TRUE)
  means_object <- means
  means <- as.data.frame(means)

  model_data <- insight::get_data(model, verbose = FALSE)
  if (is.null(model_data)) {
    insight::format_error(
      "`show_data = TRUE` needs the model data, which could not be retrieved."
    )
  }
  if (!is.character(response) || length(response) != 1L) {
    insight::format_error(
      paste(
        "`show_data = TRUE` needs the name of the response, but the contrasts",
        "do not carry it. Please obtain the contrasts with `estimate_contrasts()`."
      )
    )
  }
  if (startsWith(response, "cbind(")) {
    insight::format_error(
      paste0(
        "`show_data = TRUE` cannot draw the response ",
        response,
        ". In a binomial model, a cbind() response holds counts of successes ",
        "and failures, which are not one value per observation."
      )
    )
  }
  missing_columns <- setdiff(c(response, x_name, by_vars), names(model_data))
  if (length(missing_columns)) {
    insight::format_error(
      paste0(
        "`show_data = TRUE` needs the column(s) ",
        toString(missing_columns),
        " in the model data, but the data lack them."
      )
    )
  }

  .check_raw_response_scale(model, contrasts, means_object, response)
  .check_raw_predict(contrasts, means_object)

  # observations only for the `by` levels that the contrasts have; a numeric
  # `by` variable keeps the check below
  categorical_by <- by_vars[
    !vapply(by_vars, function(i) is.numeric(model_data[[i]]), logical(1))
  ]
  if (length(categorical_by)) {
    keep <- .by_key(model_data, categorical_by) %in%
      .by_key(as.data.frame(contrasts), categorical_by)
    model_data <- model_data[keep, , drop = FALSE]
  }

  # each data value must have its place on the x axis and in the facets
  for (i in c(x_name, by_vars)) {
    lacking <- setdiff(
      unique(.value_key(model_data[[i]])),
      .value_key(means[[i]])
    )
    if (length(lacking)) {
      insight::format_error(
        paste0(
          "`show_data = TRUE` needs each value of ",
          i,
          " in the data among the values in the estimated means, but the ",
          "means lack ",
          if (is.numeric(model_data[[i]])) {
            paste0(
              length(lacking),
              " of them. ",
              i,
              " is numeric, so the means hold only the values they were ",
              "estimated at. Please convert it to a factor."
            )
          } else {
            paste0(
              toString(lacking),
              ". Please obtain the means for all levels."
            )
          }
        )
      )
    }
  }

  x_labels <- do.call(
    paste,
    c(
      lapply(x_name, function(i) {
        # numeric values take the labels of the equal values in the means,
        # e.g. 100000L in the data and 1e5 in the means
        if (is.numeric(model_data[[i]]) && is.numeric(means[[i]])) {
          as.character(means[[i]])[
            match(as.numeric(model_data[[i]]), as.numeric(means[[i]]))
          ]
        } else {
          as.character(model_data[[i]])
        }
      }),
      sep = ", "
    )
  )
  numeric_x <- is.numeric(dataplot$geom_pointrange$x)
  x_levels <- levels(dataplot$geom_pointrange$x)

  # in joint contrasts, each combination of levels must be on the x axis
  lacking <- if (numeric_x) {
    character()
  } else {
    setdiff(unique(x_labels), x_levels)
  }
  if (length(lacking)) {
    insight::format_error(
      paste0(
        "`show_data = TRUE` needs each combination of levels of ",
        toString(x_name),
        " in the data among the estimated means, but the means lack ",
        toString(lacking),
        ". Please obtain the means for all combinations."
      )
    )
  }

  # with `by`, each combination must have its place in its facet
  if (length(by_vars)) {
    cells <- c(x_name, by_vars)
    cell_key <- function(d) {
      do.call(
        paste,
        c(lapply(cells, function(i) .value_key(d[[i]])), sep = ", ")
      )
    }
    lacking <- setdiff(unique(cell_key(model_data)), cell_key(means))
    if (length(lacking)) {
      insight::format_error(
        paste0(
          "`show_data = TRUE` needs each combination of levels of ",
          toString(cells),
          " in the data among the estimated means, but the means lack ",
          toString(lacking),
          ". Please obtain the means for all combinations."
        )
      )
    }
  }
  y <- .binary_as_numeric(model_data[[response]])

  # the `by` columns get the names and types of the means in `dataplot`
  for (j in seq_along(by_vars)) {
    target <- dataplot$geom_pointrange[[info$by[j]]]
    values <- model_data[[by_vars[j]]]
    model_data[[by_vars[j]]] <- NULL
    model_data[[info$by[j]]] <- if (is.factor(target)) {
      factor(as.character(values), levels = levels(target))
    } else if (is.character(target)) {
      as.character(values)
    } else if (is.double(target)) {
      # facets do not match integer data to double means
      as.double(values)
    } else {
      values
    }
  }

  model_data$x <- if (numeric_x) {
    as.numeric(model_data[[x_name]])
  } else {
    factor(x_labels, levels = x_levels)
  }
  model_data$y <- y
  model_data
}


# the response must be untransformed, or log-transformed with contrasts and
# means back-transformed to the raw scale
#' @keywords internal
.check_raw_response_scale <- function(model, contrasts, means, response) {
  response_term <- tryCatch(
    insight::find_terms(model)$response,
    error = function(e) NULL
  )
  if (is.null(response_term)) {
    insight::format_error(
      paste0(
        "`show_data = TRUE` needs to know whether the model transforms the ",
        "response ",
        response,
        ", but its response term could not be found."
      )
    )
  }
  back_transformed <- isTRUE(attr(contrasts, "transform", exact = TRUE)) &&
    isTRUE(attr(means, "transform", exact = TRUE)) &&
    .is_log_of(response_term, response)
  # means without the attribute, such as a plain data frame, do not say
  # whether they are back-transformed
  if (
    isTRUE(attr(contrasts, "transform", exact = TRUE)) &&
      is.null(attr(means, "transform", exact = TRUE)) &&
      .is_log_of(response_term, response)
  ) {
    insight::format_error(
      paste0(
        "`show_data = TRUE` needs to know whether the means of the response ",
        response_term,
        " are back-transformed, but the means do not carry the `transform` ",
        "attribute. Please obtain the means with ",
        "`estimate_means(transform = TRUE)`."
      )
    )
  }
  if (!identical(response_term, response) && !back_transformed) {
    insight::format_error(
      paste0(
        "`show_data = TRUE` needs the contrasts and means on the scale of ",
        "the raw data of ",
        response,
        ", but the model transforms the response (",
        toString(response_term),
        "). To show the data, please fit the model to ",
        response,
        ", or to log(",
        response,
        "), log2(",
        response,
        "), or log10(",
        response,
        ") with the contrasts and means obtained with `transform = TRUE`."
      )
    )
  }
}


# whether the response term is log(), log2(), or log10() of the response
# column itself; insight reports log(y / 10) as "log" too, but its
# back-transformed means are on the scale of y / 10
#' @keywords internal
.is_log_of <- function(response_term, response) {
  term <- tryCatch(str2lang(response_term), error = function(e) NULL)
  is.call(term) &&
    is.name(term[[1]]) &&
    as.character(term[[1]]) %in% c("log", "log2", "log10") &&
    length(term) >= 2L &&
    identical(term[[2]], as.name(response))
}


# the contrasts and the means must be estimated on the response scale;
# means without the attribute (e.g. a plain data frame) are taken as
# estimated like the contrasts
#' @keywords internal
.check_raw_predict <- function(contrasts, means) {
  estimates <- list(contrasts = contrasts, means = means)
  for (i in names(estimates)) {
    predict_type <- attr(estimates[[i]], "predict", exact = TRUE)
    if (i == "means" && is.null(predict_type)) {
      predict_type <- "response"
    }

    if (
      !isTRUE(
        predict_type %in%
          c(
            "prediction",
            "response",
            "expectation",
            "invlink(link)",
            "inverse_link"
          )
      )
    ) {
      insight::format_error(
        paste0(
          "`show_data = TRUE` needs the contrasts and means on the scale of ",
          "the response, but the ",
          i,
          if (is.null(predict_type)) {
            " do not say which `predict` they were estimated with"
          } else {
            paste0(
              " were estimated with `predict = \"",
              toString(predict_type),
              "\"`"
            )
          },
          ". Please obtain the contrasts and means with ",
          "`predict = \"response\"`."
        )
      )
    }
  }
}


# a binary response is drawn as 0 and 1, like the probabilities
#' @keywords internal
.binary_as_numeric <- function(y) {
  if (is.logical(y)) {
    as.numeric(y)
  } else if (is.factor(y) && nlevels(y) == 2L) {
    as.numeric(y == levels(y)[2])
  } else {
    y
  }
}
