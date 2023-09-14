#' Plot tabulated data.
#'
#' @param x Object created by `datawizard::data_tabulate()`.
#' @param label_values Logical. Should values and percentages be displayed at the
#'   top of each bar.
#' @param show_na Should missing values be dropped? Can be `"if_any"` (default) to show
#'   the missing category only if any missing values are present, `"always"` to
#'   always show the missing category, or `"never"` to never show the missing
#'   category.
#' @param na_label The label given to missing values when they are shown.
#' @param error_bar Logical. Should error bars be displayed?
#'   If `TRUE`, confidence intervals computed using the Wilson method are shown.
#'   See Brown et al. (2001) for details.
#' @param ci Confidence Interval (CI) level. Defaults to `0.95` (`95%`).
#' @param fill_col Color to use for category columns (default: `"#87CEFA"`).
#' @param color_error_bar Color to use for error bars (default: `"#607B8B"`).
#' @param ... Unused
#'
#' @references
#' Brown, L. D., Cai, T. T., & Dasgupta, A. (2001).
#' Interval estimation for a binomial proportion.
#' _Statistical Science_, _16_(2), 101-133. \doi{10.1214/ss/1009213286}
#'
#' @rdname plot.dw_data_tabulate
#' @export

plot.dw_data_tabulates <- function(x, label_values = TRUE,
                                   show_na = c("if_any", "always", "never"),
                                   na_label = "(Missing)",
                                   error_bar = TRUE,
                                   ci = 0.95,
                                   fill_col = "#87CEFA",
                                   color_error_bar = "#607B8B",
                                   ...) {
  show_na <- match.arg(show_na, choices = c("if_any", "always", "never"))
  if (length(x) == 1L) {
    plot.dw_data_tabulate(
      x[[1]],
      label_values = label_values,
      show_na = show_na,
      na_label = na_label,
      error_bar = error_bar,
      ci = ci,
      fill_col = fill_col,
      color_error_bar = color_error_bar
    )
  } else {
    lapply(
      x,
      plot.dw_data_tabulate,
      label_values = label_values,
      show_na = show_na,
      na_label = na_label,
      error_bar = error_bar,
      ci = ci,
      fill_col = fill_col,
      color_error_bar = color_error_bar
    )
  }
}

#' @rdname plot.dw_data_tabulate
#'
#' @export

plot.dw_data_tabulate <- function(x, label_values = TRUE,
                                  show_na = c("if_any", "always", "never"),
                                  na_label = "(Missing)",
                                  error_bar = TRUE,
                                  ci = 0.95,
                                  fill_col = "#87CEFA",
                                  color_error_bar = "#607B8B",
                                  ...) {
  show_na <- match.arg(show_na, choices = c("if_any", "always", "never"))
  dat <- as.data.frame(x)

  if (show_na == "if_any") {
    if (anyNA(dat$Value)) {
      show_na <- ifelse(dat[is.na(dat$Value), "N"] > 0, "always", "never")
    } else {
      show_na <- "never"
    }
  }

  if (show_na == "never") {
    dat <- dat[!is.na(dat$Value), ]
    dat$output <- dat[[which(startsWith(names(dat), "Valid"))]]
  } else {
    dat$output <- dat[[which(startsWith(names(dat), "Raw"))]]

    # deal with missing values
    dat$Value <- as.character(dat$Value)
    dat$Value[is.na(dat$Value)] <- na_label
    dat$Value <- factor(
      dat$Value,
      levels = c(setdiff(dat$Value, na_label), na_label)
    )
  }

  if (isTRUE(error_bar)) {
    total_n <- sum(dat$N)
    props <- dat$output / 100
    dat <- cbind(dat, CI = ci, .wilson_ci(prop = props, total_n = total_n, ci = ci) * total_n)
    dat$label <- paste0(dat$N, " (", round(dat$output, 2), "%)")
  } else {
    dat$label <- paste0(dat$N, "\n(", round(dat$output, 2), "%)")
  }

  out <- ggplot2::ggplot(dat) +
    ggplot2::aes(x = .data$Value, y = .data$N) +
    ggplot2::geom_col(fill = fill_col) +
    ggplot2::labs(title = unique(dat$Variable)) +
    theme_modern()

  if (isTRUE(label_values)) {
    if (isTRUE(error_bar)) {
      out <- out +
        ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -1, hjust = 1.2) +
        ggplot2::coord_cartesian(ylim = c(0, max(dat$CI_high)))
    } else {
      out <- out +
        ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.5) +
        ggplot2::coord_cartesian(ylim = c(0, max(dat$N) * 1.2))
    }
  }

  # add confidence intervals for frequencies
  if (isTRUE(error_bar)) {
    out <- out +
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$CI_low, ymax = .data$CI_high),
        color = color_error_bar
      )
  }

  out
}

.wilson_ci <- function(prop, total_n, ci = 0.95) {
  z <- stats::qnorm((1 - ci) / 2, lower.tail = FALSE)
  z2 <- z^2
  p1 <- prop + 0.5 * z2 / total_n
  p2 <- z * sqrt((prop * (1 - prop) + 0.25 * z2 / total_n) / total_n)
  p3 <- 1 + z2 / total_n
  CI_low <- (p1 - p2) / p3
  CI_high <- (p1 + p2) / p3
  data.frame(CI_low = CI_low, CI_high = CI_high)
}
