#' @export
data_plot.n_factors <- function(x, data = NULL, type = "bar", ...){

  s1 <- summary(x)

  if ("n_Factors" %in% names(x)) {
    var <- "n_Factors"
    lab <- "factors"
  } else {
    var <- "n_Clusters"
    lab <- "clusters"
  }

  s2 <- data.frame(n_Methods = rep(0, max(x[[var]])))

  if (type == "line") {
    s1[[var]] <- as.factor(s1[[var]])
    s2[[var]] <- factor(1:max(x[[var]]))
  } else {
    s2[[var]] <- 1:max(x[[var]])
  }

  dataplot <- rbind(s1, s2[!s2[[var]] %in% s1[[var]], ])

  if (type == "line") {
    dataplot$x <- factor(dataplot$n_Factors, levels = rev(sort(levels(dataplot$n_Factors))))
    dataplot$group <- "0"
    dataplot$group[which.max(dataplot$n_Methods)] <- "1"
  } else if (type == "area") {
    dataplot$x <- dataplot$n_Factors
  } else {
    dataplot <- dataplot[order(dataplot[[var]]), ]
    dataplot$x <- dataplot[[var]]
    dataplot$fill <- "Not-optimal"
    dataplot$fill[which.max(dataplot$n_Methods)] <- "Optimal"
  }

  dataplot$y <- dataplot$n_Methods / sum(dataplot$n_Methods)
  rownames(dataplot) <- NULL

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
plot.see_n_factors <- function(x, data = NULL, type = c("bar", "line", "area"), ...) {
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
  } else if (type == "line") {
    ggplot(x, aes(x = .data$x, y = .data$y, colour = .data$group)) +
      geom_segment(aes(y = 0, xend = .data$x, yend = .data$y)) +
      geom_point() +
      coord_flip() +
      guides(colour = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      add_plot_attributes(x)
  } else {
    ggplot(x, aes(x = .data$x, y = .data$y, fill = .data$fill)) +
      geom_bar(stat = "identity") +
      guides(fill = FALSE) +
      scale_y_continuous(labels = scales::percent) +
      add_plot_attributes(x) +
      scale_x_continuous(breaks = 1:max(x$x)) +
      scale_fill_manual(values = c("black", "red"))
  }
}

#' @export
plot.see_n_clusters <- plot.see_n_factors