#' @importFrom stats reshape
#' @export
data_plot.cluster_analysis <- function(x, data = NULL, ...) {
  dat <- .retrieve_data(x)

  dataplot <- stats::reshape(
    dat,
    idvar = ".id",
    timevar = "Group",
    v.names = "Z_Score",
    varying = 2:ncol(dat),
    direction = "long"
  )

  dataplot$Group <- as.factor(dataplot$Group)
  dataplot$Term <- factor(dataplot$Term, levels = unique(dat$Term))

  attr(dataplot, "info") <- list("xlab" = NULL,
                                 "ylab" = "Mean Z-Score",
                                 "legend_fill" = "Cluster",
                                 "title" = "Cluster Analysis")

  class(dataplot) <- c("data_plot", "see_cluster_analysis", class(dataplot))
  dataplot
}



# Plot --------------------------------------------------------------------
#' @rdname data_plot
#' @export
plot.see_cluster_analysis <- function(x, data = NULL, n_columns = NULL, size = .6, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  p <- ggplot(x, aes(x = .data$Term, y = .data$Z_Score, fill = .data$Group)) +
    geom_bar(stat = "identity", position = position_dodge(size + .1), width = size) +
    geom_hline(aes(yintercept = 0), linetype = "dotted") +
    add_plot_attributes(x)

  if (!is.null(n_columns)) {
    p <- p + facet_wrap(~ Group, ncol = n_columns)
  }

  p
}
