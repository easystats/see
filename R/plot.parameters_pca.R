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

  # Title
  if (inherits(x, "parameters_efa")) {
    title <- "Factor Analysis"
  } else {
    title <- "Principal Component Analysis"
  }

  rotation_name <- attr(x, "rotation", exact = TRUE)

  if (rotation_name == "none") {
    title <- paste("Loadings from", title, "(no rotation)")
  } else {
    title <- paste0("Rotated loadings from ", title, " (", rotation_name, ")")
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
#' The `plot()` method for the `parameters::principal_components()` function.
#'
#' @param text_color Character specifying color of text labels.
#' @inheritParams data_plot
#' @inheritParams plot.see_bayesfactor_parameters
#' @inheritParams plot.see_check_outliers
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
#' @importFrom ggplot2 .data
#' @export
plot.see_parameters_pca <- function(x,
                                    type = c("bar", "line"),
                                    size_text = 3.5,
                                    text_color = "black",
                                    size = 1,
                                    show_labels = TRUE,
                                    ...) {
  type <- match.arg(type)

  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  if (missing(size)) {
    size <- switch(type,
      "bar" = 0.6,
      "line" = 1,
      1
    )
  }

  p <- ggplot(
    as.data.frame(x),
    aes(
      y = .data$Variable,
      x = abs(.data$y),
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
      geom_segment(aes(x = 0, yend = .data$Variable, xend = abs(.data$y)), linewidth = size) +
      geom_point(size = 2 * size) +
      scale_color_gradientn(colours = c("#cd201f", "#ffffff", "#0077B5"), limits = c(-1, 1))
  }

  if (isTRUE(show_labels)) {
    p <- p +
      geom_text(
        aes(x = abs(.data$y), label = round(.data$y, 2)),
        color = text_color,
        size = size_text,
        nudge_y = 0.15
      )
  }

  p <- p +
    guides(fill = "none", color = "none") +
    scale_x_continuous(
      limits = c(0, 1.25),
      breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25),
      labels = c("0", "0.25", "0.5", "0.75", "1", "")
    ) +
    facet_wrap(~Component) +
    add_plot_attributes(x)

  p
}


#' @export
plot.see_parameters_efa <- plot.see_parameters_pca
