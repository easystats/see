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
plot.see_estimate_contrasts <- function(x, data = NULL, ...) {
  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = data)
  }

  p <- ggplot2::ggplot() +
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
