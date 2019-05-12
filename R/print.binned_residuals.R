#' @importFrom graphics plot
#' @importFrom scales percent
#' @export
print.binned_residuals_see <- function(x, ...) {
  x$se.lo <- -x$se
  if (length(unique(x$group)) > 1)
    ltitle <- "Within error bounds"
  else
    ltitle <- NULL

  term <- attr(x, "term", exact = TRUE)

  if (is.null(term))
    xtitle <- sprintf("Estimated Probability of %s", attr(x, "resp_var", exact = TRUE))
  else
    xtitle = term

  p <- ggplot(data = x, aes_string(x = "xbar")) +
    geom_abline(slope = 0, intercept = 0, colour = "grey80")

  if (!is.null(term)) {
    p <- p +
      stat_smooth(
        aes_string(y = "ybar"),
        method = "loess",
        se = FALSE,
        colour = "#00b159",
        size = .5
      )
  }

  p <- p +
    geom_ribbon(aes_string(ymin = -Inf, ymax = "se.lo"), alpha = .1 , fill = "grey70") +
    geom_ribbon(aes_string(ymin = "se", ymax = Inf), alpha = .1 , fill = "grey70") +
    geom_line(aes_string(y = "se"), colour = "grey70") +
    geom_line(aes_string(y = "se.lo"), colour = "grey70") +
    theme_bw() +
    scale_color_manual(values = c("#d11141", "#00aedb")) +
    labs(
      y = "Average residual",
      x = xtitle,
      colour = ltitle
    )

  if (is.null(term)) {
    p <- p + scale_x_continuous(labels = scales::percent)
  }

  if (is.null(ltitle)) {
    p <- p + geom_point(aes_string(y = "ybar"))
  } else {
    p <- p + geom_point(aes_string(y = "ybar", colour = "group"))
  }

  suppressWarnings(graphics::plot(p))

}
