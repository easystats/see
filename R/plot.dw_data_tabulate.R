#' Plot tabulated data.
#'
#' @param x Object created by `datawizard::data_tabulate()`.
#' @param value_lab Boolean. Should values and percentages be displayed at the
#'  top of each bar.
#' @param remove_na Boolean. Should missing values be dropped?
#' @param na_label The label given to missing values (only when
#'  `remove_na = FALSE`).
#' @param error_bar Boolean. Should error bars be displayed?
#' @param ... Unused
#'
#' @rdname plot.dw_data_tabulate
#' @export

plot.dw_data_tabulates <- function(x, value_lab = TRUE, remove_na = FALSE,
                                   na_label = "(Missing)", error_bar = TRUE,
                                   ...) {
  if (length(x) == 1) {
    plot.dw_data_tabulate(
      x[[1]], value_lab = value_lab, remove_na = remove_na,
      na_label = na_label, error_bar = error_bar
    )
  } else {
    lapply(x, plot.dw_data_tabulate,
           value_lab = value_lab, remove_na = remove_na,
           na_label = na_label, error_bar = error_bar
    )
  }
}

#' @rdname plot.dw_data_tabulate
#'
#' @export

plot.dw_data_tabulate <- function(x, value_lab = TRUE, remove_na = FALSE,
                                  na_label = "(Missing)", error_bar = TRUE,
                                  ...) {
  dat <- as.data.frame(x)

  if (isTRUE(remove_na)) {
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
    rel_frq <- dat$output / 100
    ci <- 1.96 * suppressWarnings(sqrt(rel_frq * (1 - rel_frq) / total_n))
    dat$upper.ci <- total_n * (rel_frq + ci)
    dat$lower.ci <- total_n * (rel_frq - ci)
    dat$label <- paste0(dat$N, " (", round(dat$output, 2), "%)")
  } else {
    dat$label <- paste0(dat$N, "\n(", round(dat$output, 2), "%)")
  }

  out <- ggplot(dat, aes(x = .data$Value, y = .data$N)) +
    geom_col() +
    labs(title = unique(dat$Variable)) +
    theme_modern()

  if (isTRUE(value_lab)) {
    if (isTRUE(error_bar)) {
      out <- out +
        geom_text(aes(label = .data$label), vjust = -1, hjust = 1.2) +
        ylim(c(0, max(dat$N) * 1.5))
    } else {
      out <- out +
        geom_text(aes(label = .data$label), vjust = -0.5) +
        ylim(c(0, max(dat$N) * 1.2))
    }
  }

  # add confidence intervals for frequencies
  if (isTRUE(error_bar)) {
    out <- out +
      geom_errorbar(
        aes(ymin = .data$lower.ci, ymax = .data$upper.ci),
        width = 0.5, color = "darkblue"
      )
  }

  out
}
