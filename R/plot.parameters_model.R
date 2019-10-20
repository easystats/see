#' @importFrom bayestestR reshape_ci
#' @rdname data_plot
#' @export
plot.see_parameters_model <- function(x, show_intercept = FALSE, point_size = .8, ...) {
  ## TODO check for brms models, "Intercept" may be named differently
  if (!show_intercept) x <- x[x$Parameter != "(Intercept)", ]

  if (!any(grepl("Coefficient", colnames(x), fixed = TRUE))) {
    colnames(x)[which.min(c("Median", "Mean", "Map") %in% colnames(x))] <- "Coefficient"
  }

  if (sum(grepl("^CI_low", colnames(x))) > 1) {
    x <- bayestestR::reshape_ci(x)
    x$CI <- as.character(x$CI)
    p <- ggplot(x, aes(x = .data$Parameter, y = .data$Coefficient, color = .data$CI)) +
      geom_hline(aes(yintercept = 0), linetype = "dotted") +
      geom_pointrange(
        aes(ymin = .data$CI_low, ymax = .data$CI_high),
        size = point_size,
        position = position_dodge(1 / length(unique(x$CI)))
      ) +
      coord_flip() +
      theme_modern() +
      scale_color_material()
  } else {
    x$group <- as.factor(x$Coefficient < 0)
    p <- ggplot(x, aes(x = .data$Parameter, y = .data$Coefficient, color = .data$group)) +
      geom_hline(aes(yintercept = 0), linetype = "dotted") +
      geom_pointrange(aes(ymin = .data$CI_low, ymax = .data$CI_high), size = point_size) +
      coord_flip() +
      theme_modern(legend.position = "none") +
      scale_color_material()
  }


  if ("Component" %in% colnames(x) && length(unique(x$Component)) > 1) {
    p <- p + facet_wrap(~Component, ncol = 1, scales = "free")
  }

  p + labs(x = "Estimate", y = "Parameter", colour = "CI")
}