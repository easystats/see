#' @export
data_plot.n_factors <- function(x, data = NULL, type = "bar", ...) {
  s1 <- summary(x)

  if ("n_Factors" %in% names(x)) {
    var <- "n_Factors"
    lab <- "factors"
  } else {
    var <- "n_Clusters"
    lab <- "clusters"
  }

  s2 <- data.frame(n_Methods = rep(0, max(x[[var]])))

  if (type == "line") {
    s1[[var]] <- as.factor(s1[[var]])
    s2[[var]] <- factor(1:max(x[[var]]))
  } else {
    s2[[var]] <- 1:max(x[[var]])
  }

  dataplot <- rbind(s1, s2[!s2[[var]] %in% s1[[var]], ])

  if (type == "line") {
    dataplot$x <- factor(dataplot[[var]], levels = rev(sort(levels(dataplot[[var]]))))
    dataplot$group <- "0"
    dataplot$group[which.max(dataplot$n_Methods)] <- "1"
  } else if (type == "area") {
    dataplot$x <- dataplot[[var]]
  } else {
    dataplot <- dataplot[order(dataplot[[var]]), ]
    dataplot$x <- dataplot[[var]]
    dataplot$fill <- "Not-optimal"
    dataplot$fill[which.max(dataplot$n_Methods)] <- "Optimal"
  }

  dataplot$y <- dataplot$n_Methods / sum(dataplot$n_Methods)
  rownames(dataplot) <- NULL

  if (type == "line") {
    attr(dataplot, "info") <- list(
      "ylab" = paste("Number of", lab),
      "xlab" = "Consensus between methods",
      "title" = paste("How many", lab, "to retain")
    )
  } else {
    attr(dataplot, "info") <- list(
      "xlab" = paste("Number of", lab),
      "ylab" = "Consensus between methods",
      "title" = paste("How many", lab, "to retain")
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
#' @param size Depending on `type`, a numeric value specifying size of bars,
#'   lines, or segments.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examplesIf require("parameters") && require("nFactors")
#' data(mtcars)
#' result <- n_factors(mtcars, type = "PCA")
#' result
#' plot(result, type = "line")
#' @importFrom ggplot2 .data
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
      "bar" = 0.7,
      "line" = 1,
      1
    )
  }

  if (type == "area") {
    ggplot(x, aes(x = .data$x, y = .data$y)) +
      geom_area(fill = flat_colors("grey")) +
      geom_segment(
        aes(
          x = .data$x[which.max(.data$y)],
          xend = .data$x[which.max(.data$y)],
          y = 0,
          yend = max(.data$y)
        ),
        color = flat_colors("red"),
        linetype = "dashed"
      ) +
      geom_point(aes(x = .data$x[which.max(.data$y)], y = max(.data$y)),
        color = flat_colors("red")
      ) +
      scale_y_continuous(labels = .percents) +
      scale_x_continuous(breaks = 1:max(x$x)) +
      add_plot_attributes(x)
  } else if (type == "line") {
    ggplot(x, aes(y = .data$x, x = .data$y, colour = .data$group)) +
      geom_segment(aes(x = 0, yend = .data$x, xend = .data$y), linewidth = size) +
      geom_point(size = 2 * size) +
      guides(colour = "none") +
      scale_x_continuous(labels = .percents) +
      scale_color_manual(values = unname(flat_colors(c("grey", "red")))) +
      add_plot_attributes(x)
  } else {
    ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$fill)) +
      geom_bar(stat = "identity", width = size) +
      guides(fill = "none") +
      scale_y_continuous(labels = .percents) +
      add_plot_attributes(x) +
      scale_x_continuous(breaks = 1:max(x$x)) +
      scale_fill_manual(values = unname(flat_colors(c("grey", "red"))))
  }
}

#' @export
plot.see_n_clusters <- plot.see_n_factors
