#' @importFrom dplyr group_by mutate ungroup select one_of n
#' @export
data_plot.parameters_pca <- function(x, data = NULL, ...){
  x$Variable <- factor(x$Variable, levels = unique(x$Variable))

  dataplot <- as.data.frame(x)
  dataplot$Complexity <- NULL
  dataplot$Uniqueness <- NULL

  dataplot <- .reshape_to_long(dataplot, names_to = "Component", values_to = "y", columns = 2:ncol(dataplot))

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
#' @rdname data_plot
#' @importFrom rlang .data
#' @export
plot.see_parameters_pca <- function(x, text_size = 3.5, ...) {
  if (!"data_plot" %in% class(x)) {
    x <- data_plot(x)
  }

  x %>%
    as.data.frame() %>%
    ggplot(aes(
      x = .data$Variable,
      y = abs(.data$y),
      fill = .data$y
    )) +
    geom_bar(stat = "identity", width = .6, colour = NA) +
    geom_text(aes(y = -.15, label = round(.data$y, 2)), size = text_size) +
    coord_flip() +
    scale_fill_gradientn(colours = c("#cd201f", "#ffffff", "#0077B5"), limits = c(-1, 1)) +
    guides(fill = FALSE) +
    scale_y_continuous(
      limits = c(-.25, 1),
      breaks = c(-.25, 0, .25, .5, .75, 1),
      labels = c("", "0", "0.25", "0.5", "0.75", "1")
    ) +
    facet_wrap(~ Component) +
    add_plot_attributes(x)
}


#' @export
plot.see_parameters_efa <- plot.see_parameters_pca
