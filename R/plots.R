#' Multiple plots side by side
#'
#' A wrapper around gridExtra::grid.arrange to plot multiple figures side by side on the same page.
#'
#' @inheritParams gridExtra::grid.arrange
#' @param tags Add tags to your subfigures.
#' @param tags_labels Character vector containing tags labels.
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' p1 <- ggplot(iris, aes(x=Petal.Length, y=Sepal.Width)) + geom_point()
#' p2 <- ggplot(iris, aes(x=Petal.Length)) + geom_density()
#'
#' plots(p1, p2)
#' plots(p1, p2, ncol=2, tags=TRUE)
#' plots(p1, p2, ncol=2, tags=TRUE, tags_labels=c("Fig. 1", "Fig. 2"))
#'
#' @importFrom gridExtra grid.arrange
#' @export
plots <- function(..., nrow=NULL, ncol=NULL, tags=FALSE, tags_labels=LETTERS){

  plot_list <- list(...)

  # Add tags
  if (tags == TRUE) {
    for (i in 1:length(plot_list)) {
      plot_list[[i]] <- plot_list[[i]] + ggplot2::labs(tag = tags_labels[i])
    }
  }

  gridExtra::grid.arrange(grobs = plot_list, nrow = nrow, ncol = ncol)
}