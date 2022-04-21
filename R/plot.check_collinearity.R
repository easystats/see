#' Plot method for multicollinearity checks
#'
#' The `plot()` method for the `performance::check_collinearity()` function.
#'
#' @inheritParams data_plot
#' @inheritParams plot.see_check_normality
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' result <- check_collinearity(m)
#' result
#' plot(result)
#' @importFrom ggplot2 .data
#' @export
plot.see_check_collinearity <- function(x,
                                        data = NULL,
                                        colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                        ...) {
  if (is.null(data)) {
    dat <- insight::compact_list(.retrieve_data(x))
  } else {
    dat <- data
  }

  if (is.null(dat)) {
    return(NULL)
  }

  dat$group <- "low"
  dat$group[dat$VIF >= 5 & dat$VIF < 10] <- "moderate"
  dat$group[dat$VIF >= 10] <- "high"

  dat <- datawizard::data_rename(
    dat,
    c("Term", "VIF", "SE_factor", "Component"),
    c("x", "y", "se", "facet")
  )

  dat <- datawizard::data_select(dat, c("x", "y", "se", "facet", "group"))

  if (insight::n_unique(dat$facet) <= 1) {
    dat$facet <- NULL
  }

  .plot_diag_vif(dat, colors = colors, ci_data = attributes(x)$CI)
}
