#' @export

plot.dw_data_tabulates <- function(x, value_lab = TRUE, remove_na = FALSE,
                                   na_label = "(Missing)") {
  lapply(x, function(dat) {
    plot.dw_data_tabulate(dat, value_lab = value_lab, remove_na = remove_na,
                          na_label = na_label)
  })
}

#' @export

plot.dw_data_tabulate <- function(x, value_lab = TRUE, remove_na = FALSE,
                                  na_label = "(Missing)") {

  dat <- as.data.frame(x)

  if (isTRUE(remove_na)) {
    dat <- dat[!is.na(dat$Value), ]
    dat$output <- dat$`Valid %`
  } else {
    dat$output <- dat$`Raw %`

    # deal with factors
    dat$Value <- as.character(dat$Value)
    dat$Value[is.na(dat$Value)] <- na_label
    dat$Value <- factor(dat$Value, levels = c(setdiff(dat$Value, na_label), na_label))
  }

  dat$label <- paste0(dat$N, "\n(", round(dat$output, 2), "%)")

  out <- ggplot(dat, aes(x = Value, y = N)) +
    geom_col() +
    labs(title = unique(dat$Variable)) +
    theme_modern()

  if (isTRUE(value_lab)) {
     out <- out +
       geom_text(aes(label = label), vjust = -0.5) +
       ylim(c(0, max(dat$N) * 1.2))
  }

  out
}

