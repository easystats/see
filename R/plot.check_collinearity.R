#' @export
plot.see_check_collinearity <- function(x, data = NULL) {
  if (is.null(data)) {
    dat <- .compact_list(.retrieve_data(x))
  } else {
    dat <- data
  }

  if (is.null(dat)) return(NULL)

  dat$group <- "low"
  dat$group[dat$VIF >= 5 & dat$VIF < 10] <- "moderate"
  dat$group[dat$VIF >= 10] <- "high"

  if (ncol(dat) == 5) {
    colnames(dat) <- c("x", "y", "se", "facet", "group")
    dat[, c("x", "y", "facet", "group")]
  } else {
    colnames(dat) <- c("x", "y", "se", "group")
    dat[, c("x", "y", "group")]
  }

  .plot_diag_vif(dat)
}

