#' @importFrom rlang .data
#' @importFrom graphics plot
#' @export
print.see_binned_residuals <- function(x, ...) {
  orig_x <- x
  x$se.lo <- -x$se
  if (length(unique(x$group)) > 1)
    ltitle <- "Within error bounds"
  else
    ltitle <- NULL

  # set defaults

  term <- attr(x, "term", exact = TRUE)
  geom_color <- attr(x, "geom_color", exact = TRUE)
  geom_size <- attr(x, "geom_size", exact = TRUE)


  if (is.null(term))
    xtitle <- sprintf("Estimated Probability of %s", attr(x, "resp_var", exact = TRUE))
  else
    xtitle = term

  if (is.null(geom_color)) geom_color <- c("#d11141", "#00aedb")
  if (is.null(geom_size)) geom_size <- 2


  p <- ggplot(data = x, aes(x = .data$xbar)) +
    geom_abline(slope = 0, intercept = 0, colour = "grey80")

  if (!is.null(term)) {
    p <- p +
      stat_smooth(
        aes(y = .data$ybar),
        method = "loess",
        se = FALSE,
        colour = "#00b159",
        size = .6
      )
  }

  p <- p +
    geom_ribbon(aes(ymin = -Inf, ymax = .data$se.lo), alpha = .1 , fill = "grey70") +
    geom_ribbon(aes(ymin = .data$se, ymax = Inf), alpha = .1 , fill = "grey70") +
    geom_line(aes(y = .data$se), colour = "grey70") +
    geom_line(aes(y = .data$se.lo), colour = "grey70") +
    theme_bw() +
    scale_color_manual(values = geom_color) +
    labs(
      y = "Average residual",
      x = xtitle,
      colour = ltitle
    )

  if (is.null(term)) {
    p <- p + scale_x_continuous(labels = .percents)
  }

  if (is.null(ltitle)) {
    p <- p + geom_point(aes(y = .data$ybar), size = geom_size)
  } else {
    p <- p + geom_point(aes(y = .data$ybar, colour = .data$group), size = geom_size)
  }

  suppressWarnings(graphics::plot(p))
  invisible(orig_x)
}
