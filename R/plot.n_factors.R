#' @export
data_plot.n_factors <- function(x, data = NULL, ...){
  s1 <- summary(x)
  s2 <- data.frame(n_Factors = factor(1:max(x$n_Factors)), n_Methods = 0)
  dataplot <- rbind(s1, s2[!s2$n_Factors %in% s1$n_Factors, ])

  dataplot$n_Factors <- factor(dataplot$n_Factors, levels = rev(sort(levels(dataplot$n_Factors))))
  dataplot$n_Methods <- dataplot$n_Methods / sum(dataplot$n_Methods)
  dataplot$group <- "0"
  dataplot$group[which.max(dataplot$n_Methods)] <- "1"

  class(dataplot) <- unique(c("data_plot", "see_n_factors", class(dataplot)))
  dataplot
}





# Plot --------------------------------------------------------------------
#' @importFrom rlang .data
#' @rdname data_plot
#' @export
plot.see_n_factors <- function(x, data = NULL, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data)
  }

  ggplot(x, aes(x = .data$n_Factors, y = .data$n_Methods, colour = .data$group)) +
    geom_segment(aes(y = 0, xend = .data$n_Factors, yend = .data$n_Methods)) +
    geom_point() +
    labs(
      x = "Number of Factors",
      y = "Agreement between methods"
    ) +
    coord_flip() +
    guides(colour = FALSE) +
    scale_y_continuous(labels = scales::percent)
}
