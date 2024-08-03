#' Plot method for check DAGs
#'
#' The `plot()` method for the `performance::check_dag()` function.
#'
#' @param x A `check_dag` object.
#' @param size_point Numeric value specifying size of point geoms.
#' @param colors Character vector of length five, indicating the colors (in
#' hex-format) for different types of variables.
#' @param which Character string indicating which plot to show. Can be either
#' `"all"`, `"current"` or `"required"`.
#' @param ... Not used.
#'
#' @return A ggplot2-object.
#'
#' @examplesIf require("ggdag", quietly = TRUE)
#' library(performance)
#' # incorrect adjustment
#' dag <- check_dag(
#'   y ~ x + b + c,
#'   x ~ b,
#'   outcome = "y",
#'   exposure = "x"
#' )
#' dag
#' plot(dag)
#' @export
plot.check_dag <- function(x, size_point = 15, colors = NULL, which = "all", ...) {
  .data <- NULL
  insight::check_if_installed(c("ggdag", "ggplot2"))
  which <- match.arg(which, choices = c("all", "current", "required"))

  p1 <- suppressWarnings(ggdag::ggdag_adjust(x, stylized = TRUE))
  p2 <- suppressWarnings(ggdag::ggdag_adjustment_set(x, shadow = TRUE, stylized = TRUE))

  # tweak data
  p1$data$type <- as.character(p1$data$adjusted)
  p1$data$type[vapply(p1$data$name, ggdag::is_collider, logical(1), .dag = x)] <- "collider"
  p1$data$type[p1$data$name == attributes(x)$outcome] <- "outcome"
  p1$data$type[p1$data$name %in% attributes(x)$exposure] <- "exposure"
  p1$data$type <- factor(p1$data$type, levels = c("outcome", "exposure", "adjusted", "unadjusted", "collider"))

  p2$data$type <- as.character(p2$data$adjusted)
  p2$data$type[vapply(p2$data$name, ggdag::is_collider, logical(1), .dag = x)] <- "collider"
  p2$data$type[p2$data$name == attributes(x)$outcome] <- "outcome"
  p2$data$type[p2$data$name %in% attributes(x)$exposure] <- "exposure"
  p2$data$type <- factor(p2$data$type, levels = c("outcome", "exposure", "adjusted", "unadjusted", "collider"))

  if (is.null(colors)) {
    point_colors <- see_colors(c("yellow", "cyan", "blue grey", "red", "orange"))
  } else if (length(colors) != 5) {
    insight::format_error("`colors` must be a character vector with five color-values.")
  } else {
    point_colors <- colors
  }
  names(point_colors) <- c("outcome", "exposure", "adjusted", "unadjusted", "collider")

  plot1 <- ggplot2::ggplot(p1$data, ggplot2::aes(x = .data$x, y = .data$y)) +
    geom_point_borderless(ggplot2::aes(fill = .data$type), size = size_point) +
    ggdag::geom_dag_edges(
      ggplot2::aes(
        xend = .data$xend,
        yend = .data$yend,
        edge_alpha = .data$adjusted
      )
    ) +
    ggdag::scale_adjusted() +
    ggdag::geom_dag_label(ggplot2::aes(label = .data$name)) +
    ggdag::theme_dag() +
    ggplot2::scale_fill_manual(values = point_colors) +
    ggplot2::ggtitle("Current model") +
    ggplot2::guides(edge_alpha = "none")

  plot2 <- ggplot2::ggplot(p2$data, ggplot2::aes(x = .data$x, y = .data$y)) +
    geom_point_borderless(ggplot2::aes(fill = .data$type), size = size_point) +
    ggdag::geom_dag_edges(
      ggplot2::aes(
        xend = .data$xend,
        yend = .data$yend,
        edge_alpha = .data$adjusted
      )
    ) +
    ggdag::scale_adjusted() +
    ggdag::geom_dag_label(ggplot2::aes(label = .data$name)) +
    ggdag::theme_dag() +
    ggplot2::scale_fill_manual(values = point_colors) +
    ggplot2::ggtitle("Required model") +
    ggplot2::guides(edge_alpha = "none")

  if (which == "all") {
    # fix legends
    plot2 <- plot2 + ggplot2::theme(legend.position = "none")
    # plot
    plots(plot1, plot2, n_rows = 1)
  } else if (which == "current") {
    plot1
  } else {
    plot2
  }
}
