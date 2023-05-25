#' Plot method for checking outliers
#'
#' The `plot()` method for the `performance::check_outliers()`
#' function.
#'
#' @param show_labels Logical. If `TRUE`, text labels are displayed.
#' @param size_text Numeric value specifying size of text labels.
#' @param rescale_distance Logical. If `TRUE`, distance values are rescaled
#'   to a range from 0 to 1. This is mainly due to better catch the differences
#'   between distance values.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' data(mtcars)
#' mt1 <- mtcars[, c(1, 3, 4)]
#' mt2 <- rbind(
#'   mt1,
#'   data.frame(mpg = c(37, 40), disp = c(300, 400), hp = c(110, 120))
#' )
#' model <- lm(disp ~ mpg + hp, data = mt2)
#' plot(check_outliers(model))
#' @export
plot.see_check_outliers <- function(x,
                                    size_text = 3.5,
                                    size_line = 0.8,
                                    dot_alpha = 0.8,
                                    colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                    rescale_distance = TRUE,
                                    type = c("dots", "bars"),
                                    show_labels = TRUE,
                                    ...) {
  type <- match.arg(type)
  influential_obs <- attributes(x)$influential_obs
  methods <- attr(x, "methods", exact = TRUE)

  if (type == "dots" && !is.null(influential_obs) && (is.null(methods) || length(methods) == 1)) {
    .plot_diag_outliers_new(
      influential_obs,
      show_labels = show_labels,
      size_text = size_text,
      size_line = size_line,
      dot_alpha_level = dot_alpha,
      colors = colors
    )
  } else {
    if (length(methods == 1)) {
      .plot_diag_outliers(x, show_labels = show_labels, size_text = size_text, rescale_distance = rescale_distance)
    } else {
      .plot_outliers_multimethod(x, rescale_distance)
    }
  }
}


#' @export
data_plot.check_outliers <- function(x, data = NULL, rescale_distance = TRUE, ...) {
  data <- attributes(x)$data
  row.names(data) <- data$Obs

  # Extract distances
  d <- data[grepl("Distance_", names(data), fixed = TRUE)]
  if (rescale_distance) {
    d <- datawizard::normalize(d, verbose = FALSE)
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
  d_long
}


.plot_diag_outliers <- function(x, show_labels = TRUE, size_text = 3.5, rescale_distance = TRUE) {
  d <- data_plot(x, rescale_distance = rescale_distance)
  d$Id <- seq_len(nrow(d))
  d$Outliers <- as.factor(attr(x, "data", exact = TRUE)[["Outlier"]])
  d$Id[d$Outliers == "0"] <- NA

  method <- switch(attr(x, "method", exact = TRUE),
    "cook" = "Cook's Distance",
    "pareto" = "Pareto",
    "mahalanobis" = "Mahalanobis Distance",
    "ics" = "Invariant Coordinate Selection",
    "mcd" = "Minimum Covariance Determinant",
    "optics" = "OPTICS",
    "iforest" = "Isolation Forest",
    "Cook's Distance"
  )

  threshold <- attr(x, "threshold", exact = TRUE)[[method]]
  rescaled <- attr(d, "rescale_distance")
  if (isTRUE(rescaled)) {
    x_lab <- paste0(method, " (rescaled range 0-1)")
  } else {
    x_lab <- method
  }

  size_text <- size_text %||% 3.5

  p <- ggplot(d, aes(x = .data$Distance, fill = .data$Outliers, label = .data$Id)) +
    geom_histogram() +
    labs(
      title = "Influential Observations",
      subtitle = "High Cook's distance might reflect potential outliers",
      x = x_lab,
      y = "Count",
      fill = NULL
    ) +
    scale_fill_manual(values = c("#2c3e50", "#c0392b")) +
    guides(fill = "none", color = "none", label = "none")

  if (!is.null(threshold) && !is.na(threshold)) {
    p <- p +
      geom_vline(
        xintercept = threshold,
        linetype = "dashed",
        color = "#c0392b"
      )
  }


  if (isTRUE(show_labels)) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(y = 2.5, size = size_text, na.rm = TRUE)
    } else {
      p <- p + geom_text(y = 2.5, size = size_text, na.rm = TRUE)
    }
  }

  p
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
    ggplot(
      data = d,
      aes(
        x = .data$Obs,
        y = .data$Distance,
        fill = .data$Method,
        group = .data$Method
      )
    ) +
      # geom_vline(xintercept = as.character(c(1, 2))) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_fill_viridis_d() +
      theme_modern() +
      labs(x = "Observation", y = y_lab, fill = "Method") +
      # Warning: Vectorized input to `element_text()` is not officially supported.
      # Results may be unexpected or may change in future versions of ggplot2.
      theme(
        axis.text.x = element_text(colour = ifelse(as.numeric(x) >= 0.5, "red", "darkgrey")),
        panel.grid.major.x = element_line(
          linetype = "dashed",
          colour = ifelse(as.numeric(x) >= 0.5, "red", "lightgrey")
        )
      )
  )
}
