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

  # the contrasted variable is on the x axis; if the contrasts have no
  # `contrast` attribute, fall back to the first column of the means
  x_name <- .clean_target_names(attr(x, "contrast"))
  if (is.null(x_name)) {
    x_name <- names(means)[1]
  }
  if (length(x_name) > 1) {
    insight::format_error(
      paste0(
        "Lighthouse plots support only one contrast variable, but the contrasts compare ",
        toString(x_name),
        " jointly."
      )
    )
  }

  # each contrast is drawn from the means of its own `by` level
  by <- .clean_target_names(attr(x, "by"))
  missing_columns <- setdiff(c(x_name, by), names(means))
  if (length(missing_columns)) {
    insight::format_error(
      paste0(
        "The estimated means lack the column(s) ",
        toString(missing_columns),
        " that the contrasts need. Please obtain the means with `estimate_means(by = c(",
        toString(dQuote(c(x_name, by), FALSE)),
        "))`."
      )
    )
  }

  y_name <- c("Median", "Mean", "MAP", "Coefficient")[
    c("Median", "Mean", "MAP", "Coefficient") %in% names(means)
  ][1]
  dataplot <- .data_contrasts_and_means(
    x,
    means,
    x_name = x_name,
    y_name = y_name,
    by = by
  )

  attr(dataplot, "info") <- list(
    xlab = x_name,
    ylab = y_name,
    title = paste0("Estimated ", y_name, "s and Contrasts"),
    by = by
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
  by = NULL
) {
  polygons <- as.data.frame(contrasts)
  polygons$group <- seq_len(nrow(polygons))
  polygons$Level1 <- as.character(polygons$Level1)
  polygons$Level2 <- as.character(polygons$Level2)

  # keep the level order of the contrasted variable on the x axis
  x_values <- means[[x_name]]
  if (is.factor(x_values)) {
    x_levels <- levels(x_values)[levels(x_values) %in% x_values]
  } else {
    x_levels <- unique(as.character(x_values))
  }

  data_means <- as.data.frame(means)
  data_means$x <- factor(as.character(data_means[[x_name]]), levels = x_levels)
  data_means$y <- data_means[[y_name]]
  data_means$Level2 <- data_means$Level1 <- as.character(data_means[[x_name]])
  data_means$Mean2 <- data_means$Mean1 <- data_means[[y_name]]
  data_means$ymin <- data_means$CI_low
  data_means$ymax <- data_means$CI_high

  # each contrast level must match exactly one row of the means
  if (anyDuplicated(data_means[c("Level1", by)])) {
    insight::format_error(
      paste0(
        "The estimated means have more than one row for some levels of ",
        toString(c(x_name, by)),
        ". Please obtain the means with `estimate_means(by = c(",
        toString(dQuote(c(x_name, by), FALSE)),
        "))`, without other variables."
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

  if (nrow(polygons) < nrow(contrasts)) {
    insight::format_error(
      paste0(
        "The estimated means have no row for some levels of ",
        toString(c(x_name, by)),
        " that the contrasts compare."
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
#' must include the contrasted variable and the `by` variables, and no other
#' variables that split their rows further.
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
