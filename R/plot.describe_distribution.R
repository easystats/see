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
#' @export
plot.see_parameters_distribution <- function(x, ...) {
  # get data
  data <- .retrieve_data(x)

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data, ...)
  }

  if (is.list(x)) {
    lapply(x, .plot_see_parameters_distribution)
  } else {
    .plot_see_parameters_distribution(x)
  }
}


.plot_see_parameters_distribution <- function(x) {
  p <- ggplot(x, aes(x = .data$x))

  if (is.factor(x$x)) {
    p <- p + geom_bar()
  } else if (.n_unique(x$x) <= 12) {
    p <- p + geom_bar()
  } else {
    p <- p + geom_histogram()
  }

  if (!is.null(attributes(x)$title)) {
    p <- p + ggtitle(attributes(x)$title)
  }

  p + labs(x = NULL, y = NULL)
}