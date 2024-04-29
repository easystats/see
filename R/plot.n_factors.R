#' @export
data_plot.n_factors <- function(x, data = NULL, type = "bar", ...) {
  s1 <- summary(x)

  if ("n_Factors" %in% names(x)) {
    variable <- "n_Factors"
    lab <- "factors"
  } else {
    variable <- "n_Clusters"
    lab <- "clusters"
  }

  s2 <- data.frame(n_Methods = rep(0, max(x[[variable]])))

  if (type == "line") {
    s1[[variable]] <- as.factor(s1[[variable]])
    s2[[variable]] <- factor(1:max(x[[variable]]))
  } else {
    s2[[variable]] <- 1:max(x[[variable]])
  }

  if ("Variance_Cumulative" %in% names(s1)) {
    s2$Variance_Cumulative <- NA
  }

  dataplot <- rbind(s1, s2[!s2[[variable]] %in% s1[[variable]], ])

  if ("Variance_Explained" %in% names(attributes(x))) {
    dataplot$Variance_Cumulative <- NULL # Remove column and re add
    dataplot <- merge(
      dataplot,
      attributes(x)$Variance_Explained[, c("n_Factors", "Variance_Cumulative")],
      by = "n_Factors"
    )
  }

  if (type == "line") {
    dataplot$x <- factor(dataplot[[variable]], levels = rev(sort(levels(dataplot[[variable]]))))
    dataplot$group <- "0"
    dataplot$group[which.max(dataplot$n_Methods)] <- "1"
  } else if (type == "area") {
    dataplot$x <- dataplot[[variable]]
  } else {
    dataplot <- dataplot[order(dataplot[[variable]]), ]
    dataplot$x <- dataplot[[variable]]
    dataplot$fill <- "Not-optimal"
    dataplot$fill[which.max(dataplot$n_Methods)] <- "Optimal"
  }

  dataplot$y <- dataplot$n_Methods / sum(dataplot$n_Methods)
  rownames(dataplot) <- NULL

  # Labels and titles -----------------------------------------------------

  n_max <- sum(dataplot$n_Methods)
  axis_lab <- paste0("% of methods (out of ", n_max, ")")

  # Inverse xlab and ylab for line plot
  if (type == "line") {
    attr(dataplot, "info") <- list(
      ylab = paste("Number of", lab),
      xlab = axis_lab
    )
  } else {
    attr(dataplot, "info") <- list(
      xlab = paste("Number of", lab),
      ylab = axis_lab
    )
  }

  attr(dataplot, "info")$title <- paste("How many", lab, "to retain")
  attr(dataplot, "info")$subtitle <- paste0("Number of ", lab, " considered optimal by various algorithm")
  if ("Variance_Cumulative" %in% names(dataplot) && type != "line") {
    attr(dataplot, "info")$subtitle <- paste0(
      attr(dataplot, "info")$subtitle,
      ". The dashed line represent the cumulative percentage of variance explained"
    )
  }

  class(dataplot) <- unique(c("data_plot", "see_n_factors", class(dataplot)))
  dataplot
}

#' @export
data_plot.n_clusters <- data_plot.n_factors



# Plot --------------------------------------------------------------------

#' Plot method for numbers of clusters to extract or factors to retain
#'
#' The `plot()` method for the `parameters::n_factors()` and `parameters::n_clusters()`
#'
#' @param type Character vector, indicating the type of plot.
#'   Options are three different shapes to illustrate the degree of consensus
#'   between dimensionality methods for each number of factors;
#'    `"bar"` (default) for a bar chart,
#'    `"line"` for a horizontal point and line chart, or
#'   `"area"` for an area chart (frequency polygon).
#' @param size Depending on `type`, a numeric value specifying size of bars,
#'   lines, or segments.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examplesIf require("nFactors")
#' data(mtcars)
#' result <- parameters::n_factors(mtcars, type = "PCA")
#' result
#'
#' plot(result) # type = "bar" by default
#' plot(result, type = "line")
#' plot(result, type = "area")
#'
#' @export
plot.see_n_factors <- function(x,
                               data = NULL,
                               type = c("bar", "line", "area"),
                               size = 1,
                               ...) {
  type <- match.arg(type)

  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = data, type = type)
  }

  if (missing(size)) {
    size <- switch(type,
      bar = 0.7,
      line = 1,
      1
    )
  }

  # Base plot
  if (type == "area") {
    segment_data <- data.frame(x_intercept = x$x[which.max(x$y)], y_max = max(x$y, na.rm = TRUE))
    p <- ggplot(x, aes(x = .data$x, y = .data$y)) +
      geom_area(fill = flat_colors("grey")) +
      geom_segment(
        data = segment_data,
        aes(
          x = x_intercept,
          xend = x_intercept,
          y = 0,
          yend = y_max
        ),
        color = flat_colors("red")
      ) +
      geom_point(
        data = segment_data,
        aes(x = x_intercept, y = y_max),
        color = flat_colors("red")
      ) +
      scale_x_continuous(breaks = 1:max(x$x, na.rm = TRUE)) +
      add_plot_attributes(x)
  } else if (type == "line") {
    p <- ggplot(x, aes(y = .data$x, x = .data$y, colour = .data$group)) +
      geom_segment(aes(x = 0, yend = .data$x, xend = .data$y), linewidth = size) +
      geom_point(size = 2 * size) +
      guides(colour = "none") +
      scale_x_continuous(labels = .percents) +
      scale_color_manual(values = unname(flat_colors(c("grey", "red")))) +
      add_plot_attributes(x)
    # If line, return plot as variance explained cannot be added due to the horizontal orientation of the plot
    return(p)
  } else {
    p <- ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$fill)) +
      geom_bar(stat = "identity", width = size) +
      guides(fill = "none") +
      add_plot_attributes(x) +
      scale_x_continuous(breaks = 1:max(x$x)) +
      scale_fill_manual(values = unname(flat_colors(c("grey", "red"))))
  }

  if ("Variance_Cumulative" %in% names(x)) {
    x$Varex_scaled <- x$Variance_Cumulative * max(x$y)
    p <- p +
      geom_line(
        data = x,
        aes(x = .data$x, y = .data$Varex_scaled, group = 1),
        linetype = "dashed"
      ) +
      scale_y_continuous(
        labels = .percents,
        sec.axis = sec_axis(~ . / max(x$y), name = "% of variance explained", labels = .percents)
      )
  } else {
    p <- p + scale_y_continuous(labels = .percents)
  }
  p
}

#' @export
plot.see_n_clusters <- plot.see_n_factors
