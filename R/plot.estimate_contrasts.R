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

  # each contrast is drawn from the means of its own `by` level
  by <- .clean_target_names(attr(x, "by"))

  # the means call to suggest keeps the target values of the contrasts
  means_terms <- c(attr(x, "contrast") %||% x_name, attr(x, "by"))
  means_call <- paste0(
    "`estimate_means(by = c(",
    toString(vapply(means_terms, deparse, character(1), USE.NAMES = FALSE)),
    "))`"
  )

  missing_columns <- setdiff(c(x_name, by), names(means))
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

  # `by` columns named like columns of the plot data are renamed
  contrasts <- as.data.frame(x)
  means <- as.data.frame(means)
  by_columns <- by
  collides <- by %in% c("x", "y", "ymin", "ymax", "group", "Mean1", "Mean2")
  if (any(collides)) {
    by_columns[collides] <- paste0(".by_", by[collides])
    names(contrasts)[match(by[collides], names(contrasts))] <- by_columns[collides]
    names(means)[match(by[collides], names(means))] <- by_columns[collides]
  }

  y_name <- c("Median", "Mean", "MAP", "Coefficient")[
    c("Median", "Mean", "MAP", "Coefficient") %in% names(means)
  ][1]
  dataplot <- .data_contrasts_and_means(
    contrasts,
    means,
    x_name = x_name,
    y_name = y_name,
    by = by_columns,
    means_call = means_call
  )

  attr(dataplot, "info") <- list(
    xlab = toString(x_name),
    ylab = y_name,
    title = paste0("Estimated ", y_name, "s and Contrasts"),
    x_name = x_name,
    by = by_columns
  )

  class(dataplot) <- c("data_plot", "see_estimate_contrasts", class(dataplot))
  dataplot
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


#' @keywords internal
.data_contrasts_and_means <- function(
  contrasts,
  means,
  x_name,
  y_name,
  by = NULL,
  means_call = NULL
) {
  polygons <- as.data.frame(contrasts)
  polygons$group <- seq_len(nrow(polygons))
  polygons$Level1 <- as.character(polygons$Level1)
  polygons$Level2 <- as.character(polygons$Level2)

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

  data_means$x <- factor(x_labels, levels = x_levels)
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
  missing <- setdiff(seq_len(nrow(contrasts)), polygons$group)
  if (length(missing)) {
    insight::format_warning(
      paste0(
        length(missing),
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

  polygons <- rbind(
    cbind(
      polygons,
      data.frame(
        x = factor(polygons$Level1, levels = x_levels),
        y = polygons$Mean1
      )
    ),
    cbind(
      polygons,
      data.frame(
        x = factor(polygons$Level2, levels = x_levels),
        y = polygons$Mean1 - polygons$CI_low
      )
    ),
    cbind(
      polygons,
      data.frame(
        x = factor(polygons$Level2, levels = x_levels),
        y = polygons$Mean1 - polygons$CI_high
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
#' variables that split their rows further. If the means have no row for the
#' levels of a contrast, the plot does not draw that contrast and gives a
#' warning.
#'
#' If you contrast two or more variables jointly, for example with
#' `contrast = c("am", "cyl")`, the x axis shows their combinations of levels,
#' such as `"auto, 4"`. The first variable changes slowest along the axis.
#'
#' @inheritParams data_plot
#' @param data The estimated means, obtained with
#'   `modelbased::estimate_means()`.
#' @param show_data Logical. If `TRUE`, the plot draws the raw data of the model
#'   as jittered points behind the lighthouses. The raw data come from
#'   `insight::get_data()` for the model stored in `x`, not from `data`. The
#'   plot stops with an error in these cases:
#'   - `x` is the output of `data_plot()`, or it does not carry its model.
#'   - The model transforms the response, for example `log(y)`. This also
#'     applies with `transform = TRUE`, because the back-transformed contrasts
#'     are then not differences on the scale of the raw data.
#'   - The contrasts or the estimated means were estimated on another scale
#'     than the response, for example with `predict = "link"`. Means without
#'     this information, such as a plain data frame, are taken to be on the
#'     same scale as the contrasts.
#'   - The model data lack the response or a contrasted or `by` variable.
#'   - The estimated means do not include each value in the data of the
#'     contrasted and `by` variables, or each combination of their values. For
#'     example, a numeric `by` variable has values that the means were not
#'     estimated at.
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

  contrasts <- x
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

  # the raw data are the first layer, behind the lighthouses
  if (show_data) {
    p <- p +
      ggplot2::geom_point(
        data = .raw_data_contrasts(contrasts, data, x),
        ggplot2::aes(x = .data$x, y = .data$y),
        position = ggplot2::position_jitter(width = 0.1, height = 0),
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

  # one panel per level, or combination of levels, of the `by` variables
  by <- attr(x, "info")$by
  if (length(by)) {
    p <- p + ggplot2::facet_wrap(by)
  }

  p
}


# the model's data, with x and y as the lighthouse plot `dataplot` draws them
#' @keywords internal
.raw_data_contrasts <- function(contrasts, means, dataplot) {
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
  by <- .clean_target_names(attr(contrasts, "by", exact = TRUE))
  response <- attr(contrasts, "response", exact = TRUE)
  means_object <- means
  means <- as.data.frame(means)

  raw <- insight::get_data(model, verbose = FALSE)
  if (is.null(raw)) {
    insight::format_error(
      "`show_data = TRUE` needs the model data, which could not be retrieved."
    )
  }
  missing_columns <- setdiff(c(response, x_name, by), names(raw))
  if (length(missing_columns)) {
    insight::format_error(
      paste0(
        "`show_data = TRUE` needs the column(s) ",
        toString(missing_columns),
        " in the model data, but the data lack them."
      )
    )
  }

  # the response must be untransformed: back-transformed contrasts of a
  # transformed response are not differences on the raw scale (for a log
  # response they are ratios), so their lighthouses do not fit the raw data
  response_term <- tryCatch(
    insight::find_terms(model)$response,
    error = function(e) NULL
  )
  if (!identical(response_term, response)) {
    insight::format_error(
      paste0(
        "`show_data = TRUE` needs the contrasts and means on the scale of ",
        "the raw data of ",
        response,
        ", but the model transforms the response",
        if (!is.null(response_term)) paste0(" (", toString(response_term), ")"),
        ". Please fit the model to the untransformed response to show the data."
      )
    )
  }

  # the contrasts and the means must be estimated on the response scale;
  # means without the attribute (e.g. a plain data frame) are taken as
  # estimated like the contrasts
  estimates <- list(contrasts = contrasts, means = means_object)
  for (i in names(estimates)) {
    predict <- attr(estimates[[i]], "predict", exact = TRUE)
    if (i == "means" && is.null(predict)) {
      predict <- "response"
    }

    if (
      !isTRUE(
        predict %in% c("prediction", "response", "expectation", "invlink(link)")
      )
    ) {
      insight::format_error(
        paste0(
          "`show_data = TRUE` needs the contrasts and means on the scale of ",
          "the response, but the ",
          i,
          if (is.null(predict)) {
            " do not say which `predict` they were estimated with"
          } else {
            paste0(" were estimated with `predict = \"", toString(predict), "\"`")
          },
          ". Please obtain the contrasts and means with ",
          "`predict = \"response\"`."
        )
      )
    }
  }

  # each data value must have its place on the x axis and in the facets
  for (i in c(x_name, by)) {
    lacking <- setdiff(
      unique(as.character(raw[[i]])),
      as.character(means[[i]])
    )
    if (length(lacking)) {
      insight::format_error(
        paste0(
          "`show_data = TRUE` needs each value of ",
          i,
          " in the data among the values in the estimated means, but the ",
          "means lack ",
          if (is.numeric(raw[[i]])) {
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
    c(lapply(x_name, function(i) as.character(raw[[i]])), sep = ", ")
  )
  x_levels <- levels(dataplot$geom_pointrange$x)

  # in joint contrasts, each combination of levels must be on the x axis
  lacking <- setdiff(unique(x_labels), x_levels)
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
  if (length(by)) {
    cells <- c(x_name, by)
    cell_key <- function(d) {
      do.call(
        paste,
        c(lapply(cells, function(i) as.character(d[[i]])), sep = ", ")
      )
    }
    lacking <- setdiff(unique(cell_key(raw)), cell_key(means))
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
  y <- raw[[response]]

  # the `by` columns get the names and types of the means in `dataplot`
  for (j in seq_along(by)) {
    target <- dataplot$geom_pointrange[[info$by[j]]]
    values <- raw[[by[j]]]
    raw[[by[j]]] <- NULL
    raw[[info$by[j]]] <- if (is.factor(target)) {
      factor(as.character(values), levels = levels(target))
    } else if (is.character(target)) {
      as.character(values)
    } else {
      values
    }
  }

  raw$x <- factor(x_labels, levels = x_levels)
  raw$y <- y
  raw
}
