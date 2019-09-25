#' @export
data_plot.n_factors <- function(x, data = NULL, type = "line", ...){
  s1 <- summary(x)

  if (type == "line") {
    s1$n_Factors <- as.factor(s1$n_Factors)
    s2 <- data.frame(n_Factors = factor(1:max(x$n_Factors)), n_Methods = 0)
  } else {
    s2 <- data.frame(n_Factors = 1:max(x$n_Factors), n_Methods = 0)
  }

  dataplot <- rbind(s1, s2[!s2$n_Factors %in% s1$n_Factors, ])

  if (type == "line") {
    dataplot$x <- factor(dataplot$n_Factors, levels = rev(sort(levels(dataplot$n_Factors))))
    dataplot$group <- "0"
    dataplot$group[which.max(dataplot$n_Methods)] <- "1"
  } else {
    dataplot$x <- dataplot$n_Factors
  }

  dataplot$y <- dataplot$n_Methods / sum(dataplot$n_Methods)
  attr(dataplot, "info") <- list("xlab" = "Number of Factors",
                                 "ylab" = "Agreement between methods",
                                 "title" = "How many factors to retain")

  class(dataplot) <- unique(c("data_plot", "see_n_factors", class(dataplot)))
  dataplot
}





# Plot --------------------------------------------------------------------
#' @importFrom rlang .data
#' @rdname data_plot
#' @export
plot.see_n_factors <- function(x, data = NULL, type = c("line", "area"), ...) {
  type <- match.arg(type)
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x, data = data, type = type)
  }

  if (type == "area") {
    ggplot(x, aes(x = .data$x, y = .data$y)) +
      geom_area(fill = "#2196F3") +
      geom_segment(aes(x = .data$x[which.max(.data$y)], xend = .data$x[which.max(.data$y)], y = 0, yend = max(.data$y)), color = "#E91E63", linetype = "dashed") +
      geom_point(aes(x = .data$x[which.max(.data$y)], y = max(.data$y)), color = "#E91E63") +
      scale_color_manual(values = c("black", "#E91E63")) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(breaks = 1:max(x$x)) +
      add_plot_attributes(x)
  } else {
    ggplot(x, aes(x = .data$x, y = .data$y, colour = .data$group)) +
      geom_segment(aes(y = 0, xend = .data$x, yend = .data$y)) +
      geom_point() +
      coord_flip() +
      guides(colour = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      add_plot_attributes(x)
  }
}

