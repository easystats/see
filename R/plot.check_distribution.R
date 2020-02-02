#' @importFrom graphics plot
#' @importFrom insight get_response
#' @importFrom stats residuals density
#' @importFrom gridExtra grid.arrange
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
    geom_point(size = point_size, position = position_dodge(.4)) +
    coord_flip() +
    labs(x = NULL, y = NULL, fill = NULL, colour = NULL, title = "Predicted Distribution of Residuals and Response") +
    scale_y_continuous(labels = .percents, expand = c(0, 0), limits = c(0, max_y)) +
    scale_color_material_d() +
    theme_lucid(legend.position = lp)

  dat1 <- as.data.frame(stats::density(stats::residuals(model)))
  dat2 <- data.frame(x = .factor_to_numeric(insight::get_response(model)))

  # this ensures that we have integers for response variables with values
  # much greater than 1 and with a large range, so we have proper bar plots
  if (diff(range(dat2$x)) > 10) dat2$x <- round(dat2$x)

  p2 <- ggplot(dat1, aes(x = .data$x, y = .data$y)) +
    geom_line(colour = "#2196F3") +
    labs(x = NULL, y = NULL, title = "Density of Residuals") +
    theme_lucid()

  p3 <- ggplot(dat2, aes(x = .data$x)) +
    geom_bar(fill = "#f44336", colour = NA) +
    labs(x = NULL, y = NULL, title = "Distribution of Response") +
    theme_lucid()

  p <- list(p1, p2, p3)

  if (panel) {
    gridExtra::grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 1), c(2, 3)))
  } else {
    lapply(p, graphics::plot)
  }
}



#' @export
plot.see_check_distribution_numeric <- function(x, point_size = 2, panel = TRUE, ...) {
  vec <- .retrieve_data(x)
  x <- x[-which(x$p_Vector == 0), ]

  dat <- data.frame(
    x = factor(x$Distribution, levels = rev(sort(unique(x$Distribution)))),
    y = x$p_Vector
  )

  # remove all zero-probabilities
  dat$y[dat$y == 0] <- NA

  # find max-range, add some tolerance
  max_y <- max(dat$y, na.rm = TRUE) * 1.1

  # default legend-position
  lp <- ifelse(isTRUE(panel), "right", "bottom")

  p1 <- ggplot(dat, aes(x = .data$x, y = .data$y)) +
    geom_linerange(aes(ymin = 0, ymax = .data$y), position = position_dodge(.4), size = .8) +
    geom_point(size = point_size, position = position_dodge(.4)) +
    coord_flip() +
    labs(x = NULL, y = NULL, fill = NULL, colour = NULL, title = "Predicted Distribution of Vector") +
    scale_y_continuous(labels = .percents, expand = c(0, 0), limits = c(0, max_y)) +
    theme_lucid(legend.position = lp)

  dat1 <- as.data.frame(stats::density(vec))
  dat2 <- data.frame(x = vec)

  p2 <- ggplot(dat1, aes(x = .data$x, y = .data$y)) +
    geom_line() +
    labs(x = NULL, y = NULL, title = "Density of Vector") +
    theme_lucid()

  p3 <- ggplot(dat2, aes(x = .data$x)) +
    geom_bar(colour = NA) +
    labs(x = NULL, y = NULL, title = "Distribution of Vector") +
    theme_lucid()

  p <- list(p1, p2, p3)

  if (panel) {
    gridExtra::grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 1), c(2, 3)))
  } else {
    lapply(p, graphics::plot)
  }
}

