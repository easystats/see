#' @export

plot.dw_data_tabulates <- function(x, value_lab = TRUE) {
  lapply(x, function(dat) {
    plot.dw_data_tabulate(dat, value_lab = value_lab)
  })
}

#' @export

plot.dw_data_tabulate <- function(x, value_lab = TRUE) {

  dat <- as.data.frame(x)
  dat$label <- paste0(dat$N, "\n(", round(dat$`Raw %`, 2), "%)")

  out <- ggplot(dat, aes(x = Value, y = N)) +
    geom_col() +
    labs(title = unique(dat$Variable))

  if (isTRUE(value_lab)) {
     out <- out +
       geom_text(aes(label = label), vjust = -0.5) +
       ylim(c(0, max(dat$N) * 1.1))
  }

  out
}

