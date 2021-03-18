#' @export
data_plot.cluster_analysis <- function(x, data = NULL, ...) {
  dat <- .retrieve_data(x)
  dataplot <- .reshape_to_long(
      dat,
      names_to = "Group",
      values_to = "Z_Score",
      columns = 2:ncol(dat)
    )

  dataplot$Group <- as.factor(dataplot$Group)
  dataplot$Term <- factor(dataplot$Term, levels = unique(dat$Term))

  attr(dataplot, "info") <- list(
    "xlab" = "Cluster Group",
    "ylab" = "Mean Z-Score",
    "legend_fill" = "Variable",
    "title" = "Cluster Analysis"
  )

  class(dataplot) <- c("data_plot", "see_cluster_analysis", class(dataplot))
  dataplot
}



# Plot --------------------------------------------------------------------

#' Plot method for computing cluster analysis
#'
#' The \code{plot()} method for the \code{parameters::cluster_analysis()} function.
#'
#' @param n_columns For models with multiple components (like fixed and random,
#'   count and zero-inflated), defines the number of columns for the
#'   panel-layout. If \code{NULL}, a single, integrated plot is shown.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_parameters_distribution
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(parameters)
#' groups <- cluster_analysis(iris[, 1:4], 3)
#' plot(groups)
#' @export
plot.see_cluster_analysis <- function(x, data = NULL, n_columns = NULL, size_bar = .6, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  p <- ggplot(x, aes(x = .data$Group, y = .data$Z_Score, fill = .data$Term)) +
    geom_bar(stat = "identity", position = position_dodge(size_bar + .1), width = size_bar) +
    geom_hline(aes(yintercept = 0), linetype = "dotted") +
    add_plot_attributes(x)

  if (!is.null(n_columns)) {
    p <- p +
      facet_wrap(~Group, ncol = n_columns, scales = "free_x") +
      scale_x_discrete(labels = NULL, breaks = NULL)
  }

  p + scale_fill_flat()
}
