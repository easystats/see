#' Plot method for checking outliers
#'
#' The \code{plot()} method for the \code{performance::check_outliers()} function.
#'
#' @param size_text Size of text labels.
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' data(mtcars)
#' mt1 <- mtcars[, c(1, 3, 4)]
#' mt2 <- rbind(
#'   mt1,
#'   data.frame(mpg = c(37, 40), disp = c(300, 400), hp = c(110, 120))
#' )
#' model <- lm(disp ~ mpg + hp, data = mt2)
#' plot(check_outliers(model))
#' @export
plot.see_check_outliers <- function(x, size_text = 3.5, ...) {
  methods <- attr(x, "methods", exact = TRUE)

  if (length(methods == 1)) {
    .plot_diag_outliers(x, size_text)
  } else {
    .plot_outliers_multimethod(x)
  }
}


#' @importFrom stats reshape
#' @importFrom effectsize normalize
#' @export
data_plot.check_outliers <- function(x, data = NULL, ...) {
  data <- attributes(x)$data
  row.names(data) <- data$Obs

  # Extract distances
  d <- data[grepl("Distance_", names(data))]
  d <- effectsize::normalize(d, verbose = FALSE)

  d_long <- stats::reshape(
    d,
    direction = "long",
    varying = list(names(d)),
    sep = "_",
    v.names = "Distance",
    timevar = "Method",
    times = names(d)
  )
  d_long$Obs <- as.factor(d_long$id)
  row.names(d_long) <- d_long$id <- NULL
  d_long$Method <- gsub("Distance_", "", d_long$Method)
  d_long
}


.plot_diag_outliers <- function(x, size_text = 3.5) {
  d <- data_plot(x)
  d$Id <- 1:nrow(d)
  d$Outliers <- as.factor(attr(x, "data", exact = TRUE)[["Outlier"]])
  d$Id[d$Outliers == "0"] <- NA
  d$Distance <- effectsize::normalize(d$Distance, verbose = FALSE)

  method <- switch(
    attr(x, "method", exact = TRUE),
    "cook" = "Cook's Distance",
    "pareto" = "Pareto",
    "mahalanobis" = "Mahalanobis Distance",
    "ics" = "Invariant Coordinate Selection",
    "mcd" = "Minimum Covariance Determinant",
    "optics" = "OPTICS",
    "iforest" = "Isolation Forest",
    "Cook's Distance"
  )

  threshold <- attr(x, "threshold", exact = TRUE)[[method]]

  if (is.null(size_text)) size_text <- 3.5

  p <- ggplot(d, aes(x = .data$Distance, fill = .data$Outliers, label = .data$Id)) +
    geom_histogram() +
    labs(
      title = "Check for Influential Observations",
      x = method,
      y = "Count",
      fill = NULL
    ) +
    scale_fill_manual(values = c("#2c3e50", "#c0392b")) +
    guides(fill = FALSE, color = FALSE, label = FALSE)

  if (!is.null(threshold) && !is.na(threshold)) {
    p <- p +
      geom_vline(
        xintercept = threshold,
        linetype = "dashed",
        color = "#c0392b"
      )
  }


  if (requireNamespace("ggrepel", quietly = TRUE))
    p <- p + ggrepel::geom_text_repel(y = 2.5, size = size_text)
  else
    p <- p + geom_text(y = 2.5, size = size_text)

  p
}


.plot_outliers_multimethod <- function(x) {
  d <- data_plot(x)
  ggplot(data = d, aes(x = .data$Obs, y = .data$Distance, fill = .data$Method, group = .data$Method)) +
    # geom_vline(xintercept = as.character(c(1, 2))) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_viridis_d() +
    theme_modern() +
    labs(x = "Observation", y = "Distance", fill = "Method") +
    theme(
      axis.text.x = element_text(colour = ifelse(as.numeric(x) >= 0.5, "red", "darkgrey")),
      panel.grid.major.x = element_line(linetype = "dashed", colour = ifelse(as.numeric(x) >= 0.5, "red", "lightgrey"))
    )
}