#' @export
data_plot.parameters_pca <- function(x, data = NULL, ...) {
  dataplot <- as.data.frame(x)
  dataplot$Complexity <- NULL
  dataplot$Uniqueness <- NULL
  dataplot$MSA <- NULL

  if ("Label" %in% colnames(dataplot)) {
    dataplot$Variable <- dataplot$Label
    dataplot$Label <- NULL
  }

  dataplot <- .reshape_to_long(
    dataplot,
    names_to = "Component",
    values_to = "y",
    columns = 2:ncol(dataplot)
  )
  dataplot$Variable <- factor(dataplot$Variable, levels = rev(unique(dataplot$Variable)))

  rotation_name <- attr(x, "rotation", exact = TRUE)

  if (rotation_name == "none") {
    title <- "Loadings from Principal Component Analysis (no rotation)"
  } else {
    title <- sprintf("Rotated loadings from Principal Component Analysis (%s-rotation)", rotation_name)
  }

  attr(dataplot, "info") <- list(
    "xlab" = "",
    "ylab" = "",
    "title" = title
  )

  class(dataplot) <- c("data_plot", "see_parameters_pca", "data.frame")
  dataplot
}

#' @export
data_plot.parameters_efa <- data_plot.parameters_pca



# Plot --------------------------------------------------------------------

#' Plot method for principal component analysis
#'
#' The \code{plot()} method for the \code{parameters::principal_components()} function.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_cluster_analysis
#' @inheritParams plot.see_check_outliers
#' @inheritParams plot.see_check_normality
#' @inheritParams plot.see_easycorrelation
#' @inheritParams plot.see_n_factors
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(parameters)
#' data(mtcars)
#' result <- principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
#' result
#' plot(result)
#' @importFrom rlang .data
#' @export
plot.see_parameters_pca <- function(x,
                                    type = c("bar", "line"),
                                    size_text = 3.5,
                                    text_color = "black",
                                    size = 1,
                                    ...) {
  type <- match.arg(type)
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  if (missing(size)) {
    size <- switch(type,
      "bar" = .6,
      "line" = 1,
      1
    )
  }

  p <- ggplot(
    as.data.frame(x),
    aes(
      x = .data$Variable,
      y = abs(.data$y),
      fill = .data$y,
      color = .data$y
    )
  )

  if (type == "bar") {
    p <- p +
      geom_bar(stat = "identity", width = size, colour = NA) +
      scale_fill_gradientn(colours = c("#cd201f", "#ffffff", "#0077B5"), limits = c(-1, 1))
  } else {
    p <- p +
      geom_segment(aes(y = 0, xend = .data$Variable, yend = abs(.data$y)), size = size) +
      geom_point(size = 2 * size) +
      scale_color_gradientn(colours = c("#cd201f", "#ffffff", "#0077B5"), limits = c(-1, 1))
  }

  p <- p +
    geom_text(
      aes(y = abs(.data$y), label = round(.data$y, 2)),
      color = text_color,
      size = size_text,
      nudge_y = .15
    ) +
    coord_flip() +
    guides(fill = FALSE, color = FALSE) +
    scale_y_continuous(
      limits = c(0, 1.25),
      breaks = c(0, .25, .5, .75, 1, 1.25),
      labels = c("0", "0.25", "0.5", "0.75", "1", "")
    ) +
    facet_wrap(~Component) +
    add_plot_attributes(x)

  p
}


#' @export
plot.see_parameters_efa <- plot.see_parameters_pca
