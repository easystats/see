#' @importFrom bayestestR estimate_density
#' @importFrom graphics plot
#' @param panel Logical, if \code{TRUE}, plots are arranged as panels; else, single plots are returned.
#' @rdname data_plot
#' @export
plot.see_check_distribution <- function(x, point_size = 2, panel = TRUE, ...) {

  model <- .retrieve_data(x)
  x <- x[-which(x$p_Residuals == 0 & x$p_Response == 0), ]

  dat <- data.frame(
    x = factor(c(x$Distribution, x$Distribution), levels = rev(sort(unique(x$Distribution)))),
    y = c(x$p_Response, x$p_Residuals),
    group = c(rep("Response", length(x$p_Response)), rep("Residuals", length(x$p_Residuals)))
  )

  # remove all zero-probabilities
  dat$y[dat$y == 0] <- NA

  # find max-range, add some tolerance
  max_y <- max(dat$y, na.rm = TRUE) * 1.1

  # default legend-position
  lp <- ifelse(isTRUE(panel), "right", "bottom")

  p1 <- ggplot(dat, aes(x = .data$x, y = .data$y, colour = .data$group)) +
    geom_linerange(aes(ymin = 0, ymax = .data$y), position = position_dodge(.4), size = .8) +
    geom_point(size = 2, position = position_dodge(.4)) +
    coord_flip() +
    labs(x = NULL, y = NULL, fill = NULL, colour = NULL, title = "Predicted Distribution of Model Family") +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, max_y)) +
    scale_color_material_d() +
    theme_lucid(legend.position = lp)

  dat1 <- bayestestR::estimate_density(.normalize(stats::residuals(model)))
  dat2 <- bayestestR::estimate_density(.normalize(insight::get_response(model)))
  dat1$grp <- "Residuals"
  dat2$grp <- "Response"

  p2 <- ggplot(rbind(dat1, dat2), aes(x = .data$x, y = .data$y, colour = .data$grp)) +
    geom_line(size = .8) +
    labs(x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL) +
    scale_color_material_d() +
    theme_lucid(legend.position = lp)

  p <- list(p1, p2)

  if (panel) {
    do.call(plots, p)
  } else {
    lapply(p, graphics::plot)
  }
}

