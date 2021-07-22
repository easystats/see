#' Plot method for effect size tables
#'
#' The `plot()` method for the `effectsize::effectsize()` function.
#'
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(effectsize)
#' m <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
#' result <- eta_squared(m)
#' plot(result)
#' @export
plot.see_effectsize_table <- function(x, ...) {
  if (!"Parameter" %in% colnames(x)) {
    x$Parameter <- seq_len(nrow(x))
  }

  x$Parameter <- factor(x$Parameter, levels = rev(unique(x$Parameter)))

  es_name <- colnames(x)[effectsize::is_effectsize_name(colnames(x))]
  es_lab <- gsub("_", " ", es_name)
  es_lab <- gsub("partial", "(partial)", es_lab)

  x$.es <- x[, es_name]

  if (all(c("CI_low", "CI_high") %in% colnames(x))) {
    CIs <- geom_errorbarh(aes(xmin = .data$CI_low, xmax = .data$CI_high), height = 0)
  } else {
    NULL
  }


  ggplot(x, aes(y = .data$Parameter, color = .data$.es > 0)) +
    CIs +
    geom_point(aes(x = .data$.es), size = 2) +
    geom_vline(xintercept = 0) +
    scale_color_manual(
      values = c("FALSE" = "green", "TRUE" = "blue"),
      guide = "none"
    ) +
    labs(x = es_lab) +
    theme_modern()
}


#' @rdname plot.see_equivalence_test
#' @export
plot.see_equivalence_test_effectsize <- function(x, ...) {
  if (!"Parameter" %in% colnames(x)) {
    x$Parameter <- seq_len(nrow(x))
  }

  x$Parameter <- factor(x$Parameter, levels = rev(unique(x$Parameter)))

  if (attr(x, "rule", exact = TRUE) == "cet") {
    title <- "Conditional Test for Practical Equivalence"
  } else {
    title <- "Test for Practical Equivalence"
  }

  if (attr(x, "rule", exact = TRUE) == "bayes") {
    subtitle <- "(Using Bayesian guidlines)"
  } else {
    subtitle <- ""
  }

  es_name <- colnames(x)[effectsize::is_effectsize_name(colnames(x))]
  es_lab <- gsub("_", " ", es_name)
  es_lab <- gsub("partial", "(partial)", es_lab)

  x$.es <- x[, es_name]

  ggplot(x, aes(y = .data$Parameter, color = .data$ROPE_Equivalence)) +
    geom_errorbarh(aes(xmin = .data$CI_low, xmax = .data$CI_high), height = 0) +
    geom_point(aes(x = .data$.es), size = 2) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = unique(attr(x, "rope")), linetype = "dashed") +
    scale_color_manual(values = c(
      Accepted = "#CD423F",
      Rejected = "#018F77",
      Undecided = "#FCDA3B"
    )) +
    labs(
      x = es_lab, color = "H0",
      title = title,
      subtitle = subtitle
    ) +
    theme_modern()
}
