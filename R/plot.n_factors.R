#' @export
data_plot.n_factors <- function(x, data = NULL, ...){

  s1 <- summary(x)

  if("n_Factors" %in% names(x)){
    var <- "n_Factors"
    lab <- "factors"
  } else{
    var <- "n_Clusters"
    lab <- "clusters"
  }

  s2 <- data.frame(n_Factors = 1:max(x[[var]]), n_Methods = 0)
  dataplot <- rbind(s1, s2[!s2[[var]] %in% s1[[var]], ])

  # Format and reorder
  rownames(dataplot) <- NULL
  dataplot <- dataplot[order(dataplot$n_Factors), ]

  # Add aes
  dataplot$x <- dataplot[[var]]
  dataplot$y <- dataplot$n_Methods / sum(dataplot$n_Methods)
  dataplot$fill <- "Not-optimal"
  dataplot$fill[which.max(dataplot$n_Methods)] <- "Optimal"

  attr(dataplot, "info") <- list("xlab" = paste("Number of", lab),
                                 "ylab" = "Agreement between methods",
                                 "title" = paste("How many", lab, "to retain"))

  class(dataplot) <- unique(c("data_plot", "see_n_factors", class(dataplot)))
  dataplot
}

#' @export
data_plot.n_clusters <- data_plot.n_factors



# Plot --------------------------------------------------------------------
#' @importFrom rlang .data
#' @rdname data_plot
#' @export
plot.see_n_factors <- function(x, data = NULL, ...) {

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$fill)) +
    geom_bar(stat="identity") +
    guides(fill = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    add_plot_attributes(x) +
    scale_x_continuous(breaks = 1:max(x$x)) +
    scale_fill_manual(values = c("black", "red"))
}

#' @export
plot.see_n_clusters <- plot.see_n_factors