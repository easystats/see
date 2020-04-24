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
#' @rdname data_plot
#' @param dispersion Logical, if \code{TRUE}, will add range of dispersion for each variable to the plot.
#' @param dispersion_alpha Transparency level of dispersion ribbon.
#' @param dispersion_color Color of dispersion ribbon.
#' @param dispersion_style Character, style of dispersion area. \code{"ribbon"} for a ribbon, \code{"curve"} for a normal-curve.
#' @export
plot.see_parameters_distribution <- function(x, dispersion = FALSE, dispersion_alpha = .3, dispersion_color = "#3498db", dispersion_style = c("ribbon", "curve"), ...) {
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
    lapply(x, .plot_see_parameters_distribution, dispersion_alpha, dispersion_color, dispersion_style, show_dispersion = dispersion)
  } else {
    .plot_see_parameters_distribution(x, dispersion_alpha, dispersion_color, dispersion_style, show_dispersion = dispersion)
  }
}



#' @importFrom stats dnorm
.plot_see_parameters_distribution <- function(x, dispersion_alpha, dispersion_color, dispersion_style, show_dispersion) {
  centrality <- attributes(x)$centrality
  dispersion <- attributes(x)$dispersion

  if (!is.null(centrality) && !is.null(dispersion)) {
    x$curve_y <- nrow(x) * stats::dnorm(x = x$x, mean = centrality, sd = dispersion)
  } else if (is.null(dispersion)) {
    dispersion_style <- "ribbon"
  } else if (is.null(centrality)) {
    show_dispersion <- FALSE
  }

  p <- ggplot(x, aes(x = .data$x))

  if (is.factor(x$x)) {
    p <- p + geom_bar()
  } else if (.n_unique(x$x) <= 12) {
    p <- p + geom_bar()
  } else {
    p <- p + geom_histogram()
  }


  if (isTRUE(show_dispersion)) {
    if (dispersion_style == "ribbon") {
      p <- p + geom_vline(xintercept = centrality, colour = dispersion_color, alpha = dispersion_alpha)
    }
    if (!is.null(dispersion)) {
      if (dispersion_style == "ribbon") {
        .range <- centrality  + c(-1, 1) * dispersion
        p <- p +
          geom_vline(xintercept = .range, linetype = "dashed", colour = dispersion_color, alpha = dispersion_alpha) +
          annotate("rect", xmin = .range[1], xmax = .range[2], ymin = 0, ymax = Inf, fill = dispersion_color, alpha = (dispersion_alpha / 3))
      } else {
        p <- p +
          geom_ribbon(aes(ymin = 0, ymax = .data$curve_y), alpha = dispersion_alpha, fill = dispersion_color, colour = NA)
      }
    }
  }


  if (!is.null(attributes(x)$title)) {
    p <- p + ggtitle(attributes(x)$title)
  }

  p + labs(x = NULL, y = NULL)
}