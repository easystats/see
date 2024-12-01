#' @export
data_plot.parameters_distribution <- function(x, data = NULL, ...) {
  if (nrow(x) == 1 && is.null(x$Variable)) {
    dataplot <- data.frame(
      x = data,
      stringsAsFactors = FALSE
    )
    attr(dataplot, "centrality") <- stats::setNames(x[[1]], colnames(x)[1])
    attr(dataplot, "dispersion") <- stats::setNames(x[[2]], colnames(x)[2])
  } else {
    dataplot <- lapply(seq_len(nrow(x)), function(i) {
      out <- data.frame(
        x = data[[i]],
        stringsAsFactors = FALSE
      )
      attr(out, "title") <- x[[1]][i]
      attr(out, "centrality") <- stats::setNames(x[[2]][i], colnames(x)[1])
      attr(out, "dispersion") <- stats::setNames(x[[3]][i], colnames(x)[2])
      out
    })
  }

  dataplot
}


# Plot --------------------------------------------------------------------
#' Plot method for describing distributions of vectors
#'
#' The `plot()` method for the `parameters::describe_distribution()`
#' function.
#'
#' @param dispersion Logical. If `TRUE`, a range of dispersion for
#'   each variable to the plot will be added.
#' @param alpha_dispersion Numeric value specifying the transparency level of dispersion ribbon.
#' @param color_dispersion Character specifying the color of dispersion ribbon.
#' @param dispersion_style Character describing the style of dispersion area.
#'   `"ribbon"` for a ribbon, `"curve"` for a normal-curve.
#' @param highlight A vector with names of categories in `x` that should be
#'   highlighted.
#' @param color_highlight A vector of color values for highlighted categories.
#'   The remaining (non-highlighted) categories will be filled with a lighter
#'   grey.
#' @param size_bar Size of bar geoms.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(parameters)
#' set.seed(333)
#' x <- sample(1:100, 1000, replace = TRUE)
#' result <- describe_distribution(x)
#' result
#' plot(result)
#' @export
plot.see_parameters_distribution <- function(x,
                                             dispersion = FALSE,
                                             alpha_dispersion = 0.3,
                                             color_dispersion = "#3498db",
                                             dispersion_style = c("ribbon", "curve"),
                                             size_bar = 0.7,
                                             highlight = NULL,
                                             color_highlight = NULL,
                                             ...) {
  # get data
  data <- .retrieve_data(x)

  # only keep variables used in "describe_distribution()"
  if (!is.null(x$Variable)) {
    data <- data[, x$Variable, drop = FALSE]
  }

  if (!inherits(x, "data_plot")) {
    x <- data_plot(x, data = data, ...)
  }

  dispersion_style <- match.arg(dispersion_style)

  if (is.list(x) && !is.data.frame(x)) {
    lapply(
      x,
      .plot_see_parameters_distribution,
      alpha_dispersion,
      color_dispersion,
      dispersion_style,
      show_dispersion = dispersion,
      size_bar = size_bar,
      highlight = highlight,
      color_highlight = color_highlight
    )
  } else {
    .plot_see_parameters_distribution(
      x,
      alpha_dispersion,
      color_dispersion,
      dispersion_style,
      show_dispersion = dispersion,
      size_bar = size_bar,
      highlight = highlight,
      color_highlight = color_highlight
    )
  }
}


.plot_see_parameters_distribution <- function(x,
                                              alpha_dispersion,
                                              color_dispersion,
                                              dispersion_style,
                                              show_dispersion,
                                              size_bar,
                                              highlight,
                                              color_highlight) {
  centrality <- attributes(x)$centrality
  dispersion <- attributes(x)$dispersion

  if (!is.null(centrality) && !is.null(dispersion) && is.numeric(x$x)) {
    x$curve_y <- nrow(x) * stats::dnorm(x = x$x, mean = centrality, sd = dispersion)
  } else if (!is.null(centrality) && is.null(dispersion)) {
    dispersion_style <- "ribbon"
  } else if (is.null(centrality)) {
    show_dispersion <- FALSE
  }

  if (!is.null(highlight)) {
    highlight <- highlight[highlight %in% x$x]
    if (length(highlight) > 0L) {
      x$highlight <- "no_highlight"
      for (i in highlight) {
        x$highlight[x$x == i] <- i
      }
    }
  }

  if (!is.null(x$highlight)) {
    p <- ggplot(x, aes(x = .data$x, fill = highlight))
  } else {
    p <- ggplot(x, aes(x = .data$x))
  }

  if (is.factor(x$x) || is.character(x$x) || insight::n_unique(x$x) <= 12L) {
    p <- p + geom_bar(width = size_bar)
  } else if (.is_integer(x$x)) {
    p <- p +
      geom_bar(width = size_bar) +
      scale_x_continuous(n.breaks = round(insight::n_unique(x$x) / 4))
  } else {
    p <- p + geom_histogram()
  }


  if (isTRUE(show_dispersion)) {
    if (dispersion_style == "ribbon") {
      p <- p + geom_vline(
        xintercept = centrality,
        colour = color_dispersion,
        alpha = alpha_dispersion
      )
    }
    if (!is.null(dispersion)) {
      if (dispersion_style == "ribbon") {
        .range <- centrality + c(-1, 1) * dispersion
        p <- p +
          geom_vline(
            xintercept = .range,
            linetype = "dashed",
            colour = color_dispersion,
            alpha = alpha_dispersion
          ) +
          annotate(
            "rect",
            xmin = .range[1],
            xmax = .range[2],
            ymin = 0,
            ymax = Inf,
            fill = color_dispersion,
            alpha = (alpha_dispersion / 3)
          )
      } else {
        p <- p +
          geom_ribbon(
            aes(ymin = 0, ymax = .data$curve_y),
            alpha = alpha_dispersion,
            fill = color_dispersion,
            colour = NA
          )
      }
    }
  }

  if (!is.null(x$highlight)) {
    if (is.null(color_highlight)) {
      color_highlight <- palette_material("full")(insight::n_unique(x$highlight) - 1L)
    }

    names(color_highlight) <- highlight
    color_highlight <- c(color_highlight, no_highlight = "grey70")

    p <- p +
      scale_fill_manual(values = color_highlight) +
      guides(fill = "none")
  }

  if (!is.null(attributes(x)$title)) {
    p <- p + ggtitle(attributes(x)$title)
  }

  p + labs(x = NULL, y = NULL)
}
