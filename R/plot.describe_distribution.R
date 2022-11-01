#' @export
data_plot.parameters_distribution <- function(x, data = NULL, ...) {
  if (nrow(x) == 1) {
    dataplot <- data.frame(
      x = data,
      stringsAsFactors = FALSE
    )
    attr(dataplot, "centrality") <- stats::setNames(x[[1]], colnames(x)[1])
    attr(dataplot, "dispersion") <- stats::setNames(x[[2]], colnames(x)[2])
  } else {
    dataplot <- lapply(1:nrow(x), function(i) {
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
#' @param dispersion_alpha Numeric value specifying the transparency level of dispersion ribbon.
#' @param dispersion_color Character specifying the color of dispersion ribbon.
#' @param dispersion_style Character describing the style of dispersion area.
#'   `"ribbon"` for a ribbon, `"curve"` for a normal-curve.
#' @param highlight A vector with names of categories in `x` that should be
#'   highlighted.
#' @param highlight_color A vector of color values for highlighted categories.
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
                                             dispersion_alpha = 0.3,
                                             dispersion_color = "#3498db",
                                             dispersion_style = c("ribbon", "curve"),
                                             size_bar = 0.7,
                                             highlight = NULL,
                                             highlight_color = NULL,
                                             ...) {
  # get data
  data <- .retrieve_data(x)

  # only keep variables used in "describe_distribution()"
  if (!is.null(x$Variable)) {
    data <- data[x$Variable]
  }

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data, ...)
  }

  dispersion_style <- match.arg(dispersion_style)

  if (is.list(x) && !is.data.frame(x)) {
    lapply(
      x,
      .plot_see_parameters_distribution,
      dispersion_alpha,
      dispersion_color,
      dispersion_style,
      show_dispersion = dispersion,
      size_bar = size_bar,
      highlight = highlight,
      highlight_color = highlight_color
    )
  } else {
    .plot_see_parameters_distribution(
      x,
      dispersion_alpha,
      dispersion_color,
      dispersion_style,
      show_dispersion = dispersion,
      size_bar = size_bar,
      highlight = highlight,
      highlight_color = highlight_color
    )
  }
}



.plot_see_parameters_distribution <- function(x,
                                              dispersion_alpha,
                                              dispersion_color,
                                              dispersion_style,
                                              show_dispersion,
                                              size_bar,
                                              highlight,
                                              highlight_color) {
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
    if (length(highlight) > 0) {
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

  if (is.factor(x$x) || is.character(x$x) || insight::n_unique(x$x) <= 12) {
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
        colour = dispersion_color,
        alpha = dispersion_alpha
      )
    }
    if (!is.null(dispersion)) {
      if (dispersion_style == "ribbon") {
        .range <- centrality + c(-1, 1) * dispersion
        p <- p +
          geom_vline(
            xintercept = .range,
            linetype = "dashed",
            colour = dispersion_color,
            alpha = dispersion_alpha
          ) +
          annotate(
            "rect",
            xmin = .range[1],
            xmax = .range[2],
            ymin = 0,
            ymax = Inf,
            fill = dispersion_color,
            alpha = (dispersion_alpha / 3)
          )
      } else {
        p <- p +
          geom_ribbon(
            aes(ymin = 0, ymax = .data$curve_y),
            alpha = dispersion_alpha,
            fill = dispersion_color,
            colour = NA
          )
      }
    }
  }

  if (!is.null(x$highlight)) {
    if (is.null(highlight_color)) {
      highlight_color <- palette_material("full")(insight::n_unique(x$highlight) - 1)
    }

    names(highlight_color) <- highlight
    highlight_color <- c(highlight_color, "no_highlight" = "grey70")

    p <- p +
      scale_fill_manual(values = highlight_color) +
      guides(fill = "none")
  }

  if (!is.null(attributes(x)$title)) {
    p <- p + ggtitle(attributes(x)$title)
  }

  p + labs(x = NULL, y = NULL)
}
