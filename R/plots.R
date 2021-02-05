#' Multiple plots side by side
#'
#' A wrapper around gridExtra::grid.arrange to plot multiple figures side by side on the same page.
#'
#' @inheritParams gridExtra::grid.arrange
#' @param tags Add tags to your subfigures. Can be \code{FALSE} (no tags), \code{TRUE} (letter tags) or character vector containing tags labels.
#' @param n_columns Number of columns to align plots.
#' @param n_rows Number of rows to align plots.
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' p1 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
#'   geom_point()
#' p2 <- ggplot(iris, aes(x = Petal.Length)) +
#'   geom_density()
#'
#' plots(p1, p2)
#' plots(p1, p2, n_columns = 2, tags = TRUE)
#' plots(p1, p2, n_columns = 2, tags = c("Fig. 1", "Fig. 2"))
#' @export
plots <- function(..., n_rows = NULL, n_columns = NULL, tags = FALSE) {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' required for this function to work. Please install it.", call. = FALSE)
  }

  plot_list <- list(...)

  # Add tags
  if (length(tags) == 1) {
    if (tags == TRUE) {
      tags_labels <- LETTERS
    } else {
      tags_labels <- NULL
    }
  } else {
    if (length(tags) < length(plot_list)) {
      warning("Not enough tags labels in list. Using letters instead.")
      tags_labels <- LETTERS
    } else {
      tags_labels <- tags
    }
  }


  if (!is.null(tags_labels)) {
    for (i in 1:length(plot_list)) {
      plot_list[[i]] <- plot_list[[i]] + labs(tag = tags_labels[i])
    }
  }

  gridExtra::grid.arrange(grobs = plot_list, nrow = n_rows, ncol = n_columns)
}
