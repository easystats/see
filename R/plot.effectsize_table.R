#' @importFrom effectsize is_effectsize_name
#' @rdname data_plot
#' @export
plot.see_effectsize_table <- function(x) {
  if (!"Parameter" %in% colnames(x)) {
    x$Parameter <- factor(seq_len(nrow(x)))
  }
  es_name <- colnames(x)[effectsize::is_effectsize_name(colnames(x))]
  es_lab <- gsub("_", " ", es_name)
  es_lab <- gsub("partial", "(partial)", es_lab)

  x$.es <- x[, es_name]


  ggplot(x, aes(y = .data$Parameter, color = .data$.es > 0)) +
    geom_errorbarh(aes(xmin = .data$CI_low, xmax = .data$CI_high), height = 0.1) +
    geom_point(aes(x = .data$.es), size = 2) +
    geom_vline(xintercept = 0) +
    scale_color_manual(values = c("FALSE" = "green", "TRUE" = "blue"),
                       guide = FALSE) +
    labs(x = es_lab) +
    theme_modern()
}

#' @rdname data_plot
#' @export
plot.see_equivalence_test_effectsize <- function(x) {
  if (!"Parameter" %in% colnames(x)) {
    x$Parameter <- factor(seq_len(nrow(x)))
  }
  es_name <- colnames(x)[effectsize::is_effectsize_name(colnames(x))]
  es_lab <- gsub("_", " ", es_name)
  es_lab <- gsub("partial", "(partial)", es_lab)

  x$.es <- x[, es_name]

  ggplot(x, aes(y = .data$Parameter, color = ROPE_Equivalence)) +
    geom_errorbarh(aes(xmin = .data$CI_low, xmax = .data$CI_high), height = 0.1) +
    geom_point(aes(x = .data$.es), size = 2) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = unique(attr(x, "rope")), linetype = "dashed") +
    scale_color_manual(values = c(Accepted = "blue",
                                  Rejected = "green",
                                  Undecided = "orange")) +
    labs(x = es_lab, color = "H0") +
    theme_modern()
}