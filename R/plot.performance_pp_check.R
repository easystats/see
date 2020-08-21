#' @importFrom stats reshape
#' @export
data_plot.performance_pp_check <- function(x, ...) {
  columns <- colnames(x)
  dataplot <- stats::reshape(
    x,
    times = columns,
    timevar = "key",
    v.names = "values",
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dataplot[["values"]])) {
    dataplot[["values"]] <- as.character(dataplot[["values"]])
  }

  dataplot <- dataplot[, 1:(ncol(dataplot) - 1), drop = FALSE]
  dataplot$key[dataplot$key != "y"] <- "yrep"
  dataplot$grp <- rep(1:ncol(x), each = nrow(x))
  dataplot$alpha[dataplot$key != "y"] <- .3
  dataplot$alpha[dataplot$key == "y"] <- 1

  attr(dataplot, "info") <- list("xlab" = NULL,
                                 "ylab" = NULL,
                                 "legend_fill" = NULL,
                                 "legend_color" = NULL,
                                 "title" = "Posterior Predictive Check")

  class(dataplot) <- unique(c("data_plot", "see_performance_pp_check", class(dataplot)))
  dataplot
}


# Plot --------------------------------------------------------------------

#' Plot method for posterior predictive checks
#'
#' The \code{plot()} method for the \code{performance::pp_check()} function.
#'
#' @param line_alpha Alpha value of lines indicating \code{yrep}.
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_parameters_distribution
#'
#' @return A ggplot2-object.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' pp_check(model)
#' @export
print.see_performance_pp_check <- function(x, size_line = .7, line_alpha = .25, size_bar = 0.7, ...) {
  orig_x <- x
  check_range <- isTRUE(attributes(x)$check_range)

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  p1 <- .plot_pp_check(x, size_line, line_alpha)

  if (isTRUE(check_range)) {
    p2 <- .plot_pp_check_range(orig_x, size_bar)
    plots(p1, p2)
  } else {
    suppressWarnings(graphics::plot(p1))
  }

  invisible(orig_x)
}


#' @rdname print.see_performance_pp_check
#' @export
plot.see_performance_pp_check <- function(x, size_line = .7, line_alpha = .25, size_bar = 0.7, ...) {
  orig_x <- x
  check_range <- isTRUE(attributes(x)$check_range)

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  p1 <- .plot_pp_check(x, size_line, line_alpha)

  if (isTRUE(check_range)) {
    p2 <- .plot_pp_check_range(orig_x, size_bar)
    plots(p1, p2)
  } else {
    p1
  }
}



.plot_pp_check <- function(x, size_line, line_alpha) {
  ggplot() +
    stat_density(data = x[x$key != "y", ], mapping = aes(x = .data$values, group = .data$grp, color = .data$key), geom = "line", position = "identity", size = size_line, alpha = line_alpha) +
    stat_density(data = x[x$key == "y", ], mapping = aes(x = .data$values, group = .data$grp, color = .data$key), geom = "line", position = "identity", size = size_line * 1.1) +
    scale_y_continuous(labels = NULL) +
    scale_color_manual(values = c("y" = unname(flat_colors("dark red")), "yrep" = unname(flat_colors("grey")))) +
    labs(color = NULL) +
    add_plot_attributes(x)
}


.plot_pp_check_range <- function(x, size_bar = .7) {
  original <- data.frame(x = c(min(x$y), max(x$y)), group = c("minimum", "maximum"), color = "y", stringsAsFactors = FALSE)
  replicated <- rbind(
    data.frame(x = sapply(x[which(names(x) != "y")], min), group = "minimum", color = "yrep", stringsAsFactors = FALSE),
    data.frame(x = sapply(x[which(names(x) != "y")], max), group = "maximum", color = "yrep", stringsAsFactors = FALSE)
  )

  replicated$group <- factor(replicated$group, levels = c("minimum", "maximum"))
  original$group <- factor(original$group, levels = c("minimum", "maximum"))

  p <- ggplot(replicated, aes(x = .data$x, group = .data$group)) +
    facet_wrap(~group, scales = "free_x")

  if (.n_unique(replicated$x) <= 12) {
    p <- p + geom_bar(width = size_bar, fill = unname(flat_colors("grey")), color = NA)
  } else if (.is_integer(replicated$x)) {
    p <- p +
      geom_bar(width = size_bar, fill = unname(flat_colors("grey")), color = NA) +
      scale_x_continuous(n.breaks = round(.n_unique(replicated$x) / 4))
  } else {
    p <- p + geom_histogram(binwidth = size_bar, fill = unname(flat_colors("grey")), color = NA)
  }

  p +
    geom_vline(data = original, mapping = aes(xintercept = .data$x), color = unname(flat_colors("dark red")), size = 1) +
    labs(x = NULL, y = NULL)
}
