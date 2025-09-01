#' Plot method for checking outliers
#'
#' The `plot()` method for the `performance::check_outliers()`
#' function.
#'
#' @param type Character vector, indicating the type of plot.
#' Options are:
#' - `"dots"` (default) for a scatterplot of leverage (hat) values versus
#'   residuals, with Cook's Distance contours for evaluating influential points.
#' - `"scree"` for a scree-style plot highlighting "elbow outliers" (based on
#'   sudden increases in distance; see details).
#' - `"bars"` for a bar chart of (rescaled) outlier statistic values for each
#'   data point.
#' - `"count"` for a "histogram"-style plot of outlier, where bins represent
#'   the outliers' distance values.
#'
#' `type = "dots"` is only used for outlier plots of fitted models; for
#' outlier plots of raw data values, `type` should be one of the other options.
#' @param show_labels Logical. If `TRUE`, text labels are displayed.
#' @param size_text Numeric value specifying size of text labels.
#' @param rescale_distance Logical. If `TRUE`, distance values are rescaled
#'   to a range from 0 to 1. This is mainly due to better catch the differences
#'   between distance values.
#' @param elbow_threshold Optional scalar specifying the minimum jump in
#'   distance (between adjacent sorted observations) used to detect the elbow point.
#'   If supplied, all observations following the first jump greater than this value
#'   are flagged as outliers. If `NULL` (default), the largest jump is used
#'   automatically. Higher values yield more conservative outlier detection.
#' @param verbose Logical. If `TRUE` (default), prints a summary list of outliers.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @details When using `type = "scree"`, the function will provide a
#' scree-style distance plot that highlights two types of outliers.
#' Observations exceeding the specified threshold are shown in warm colors,
#' while observations following the largest jump ("elbow", or the specified
#' cut-off value) in the sorted distances are shown in cool colors. Elbow
#' outliers are defined based on sudden increases in distance, analogous to
#' inflection points in scree plots.
#'
#' @return A ggplot2-object.
#' @references
#' The scree plot implementation was inspired by a visualization approach
#' developed by Prof. Marina Doucerain (Université du Québec à Montréal).
#' @examples
#' library(performance)
#' data(mtcars)
#' mt1 <- mtcars[, c(1, 3, 4)]
#' mt1$ID <- row.names(mt1)
#' mt2 <- rbind(
#'   mt1,
#'   data.frame(
#'     mpg = c(37, 48), disp = c(300, 400), hp = c(110, 120),
#'     ID = c("JZ", "GP")
#'   )
#' )
#' model <- lm(disp ~ mpg + hp, data = mt2)
#' plot(check_outliers(model))
#' plot(check_outliers(mt2$mpg, method = "zscore"), type = "bar")
#' @examplesIf require("ggrepel")
#' plot(check_outliers(mt2[-3], method = "mahalanobis", ID = "ID"))
#' @export
plot.see_check_outliers <- function(
  x,
  size_text = 3.5,
  linewidth = 0.8,
  size_title = 12,
  size_axis_title = base_size,
  base_size = 10,
  alpha_dot = 0.8,
  colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
  rescale_distance = FALSE,
  type = "dots",
  elbow_threshold = NULL,
  show_labels = TRUE,
  verbose = TRUE,
  ...
) {
  # need to know the method first, because we change the default plot type
  # depending on the method
  outlier_methods <- attr(x, "method", exact = TRUE)

  # validate that the method is correct
  if (length(outlier_methods) == 0) {
    insight::format_error(
      "Invalid outlier method detected. Please ensure `check_outliers()` was called with valid parameters."
    )
  } else if (
    length(outlier_methods) == 2 && all(outlier_methods == c("optics", "optics_xi"))
  ) {
    outlier_methods <- outlier_methods[[1]]
  }

  # set default plot type depending on the method
  if ((missing(type) || is.null(type))) {
    type <- "scree"
  }

  # validate arguments
  type <- insight::validate_argument(type, c("dots", "scree", "count", "bars"))
  influential_obs <- attributes(x)$influential_obs

  if (length(outlier_methods) > 1 || type == "bars") {
    .plot_outliers_multimethod(x, rescale_distance = rescale_distance)
  } else if (type == "dots" && !is.null(influential_obs)) {
    .plot_diag_outliers_dots(
      influential_obs,
      show_labels = show_labels,
      size_text = size_text,
      linewidth = linewidth,
      size_axis_title = size_axis_title,
      size_title = size_title,
      base_size = base_size,
      alpha_dot = alpha_dot,
      colors = colors
    )
  } else if (type == "count") {
    .plot_diag_outliers_dots_count(
      x,
      show_labels = show_labels,
      size_text = size_text,
      rescale_distance = rescale_distance
    )
  } else {
    .plot_scree(
      x,
      rescale_distance = rescale_distance,
      elbow_threshold = elbow_threshold,
      verbose = verbose,
      ...
    )
  }
}


#' @export
data_plot.check_outliers <- function(
  x,
  data = NULL,
  rescale_distance = TRUE,
  ...
) {
  att <- attributes(x)
  data <- att$data
  row.names(data) <- data$Obs

  # Extract distances
  d <- data[grepl("Distance_", names(data), fixed = TRUE)]
  if (rescale_distance) {
    # Also normalize the threshold using the original values
    threshold <- att$threshold[[1]]
    mdist <- d[1]
    rescale_threshold <- (threshold - min(mdist, na.rm = TRUE)) /
      (max(mdist, na.rm = TRUE) - min(mdist, na.rm = TRUE))
    attr(d, "rescale_threshold") <- rescale_threshold
    d <- datawizard::normalize(d, verbose = FALSE)
  } else {
    rescale_threshold <- NULL
  }

  d_long <- stats::reshape(
    d,
    direction = "long",
    varying = list(names(d)),
    sep = "_",
    v.names = "Distance",
    timevar = "Method",
    times = names(d)
  )
  d_long$Obs <- as.factor(d_long$id)
  row.names(d_long) <- d_long$id <- NULL
  d_long$Method <- gsub("Distance_", "", d_long$Method, fixed = TRUE)
  attr(d_long, "rescale_distance") <- isTRUE(rescale_distance)
  attr(d_long, "rescale_threshold") <- rescale_threshold
  d_long$ID <- data$ID
  d_long
}


.plot_outliers_multimethod <- function(x, rescale_distance = TRUE) {
  d <- data_plot(x, rescale_distance = rescale_distance)

  rescaled <- attr(d, "rescale_distance")
  if (isTRUE(rescaled)) {
    y_lab <- "Distance (rescaled range 0-1)"
  } else {
    y_lab <- "Distance"
  }

  suppressWarnings(
    ggplot2::ggplot(
      data = d,
      ggplot2::aes(
        x = .data$Obs,
        y = .data$Distance,
        fill = .data$Method,
        group = .data$Method
      )
    ) +
      # geom_vline(xintercept = as.character(c(1, 2))) +
      ggplot2::geom_bar(position = "dodge", stat = "identity") +
      ggplot2::scale_fill_viridis_d() +
      theme_modern() +
      ggplot2::labs(x = "Observation", y = y_lab, fill = "Method") +
      ## FIXME: Warning: Vectorized input to `element_text()` is not officially
      ## supported. Results may be unexpected or may change in future versions
      ## of ggplot2.
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          colour = ifelse(as.numeric(x) >= 0.5, "red", "darkgrey")
        ),
        panel.grid.major.x = ggplot2::element_line(
          linetype = "dashed",
          colour = ifelse(as.numeric(x) >= 0.5, "red", "lightgrey")
        )
      ) +
      ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 2))
  )
}
