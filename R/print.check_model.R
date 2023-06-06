#' @export
print.see_check_model <- function(x,
                                  style = theme_lucid,
                                  colors = NULL,
                                  type = "density",
                                  n_columns = 2,
                                  ...) {
  # set default - we need to evaluate "type" here, because when it's passed
  # to plot(), "type" is no longer recognized as "missing()"
  plot_type <- attr(x, "type")

  if (missing(type) && !is.null(plot_type) && plot_type %in% c("density", "discrete_dots", "discrete_interval", "discrete_both")) {
    type <- plot_type
  } else {
    type <- match.arg(type, choices = c("density", "discrete_dots", "discrete_interval", "discrete_both"))
  }

  suppressWarnings(suppressMessages(plot(
    x,
    style = style,
    colors = colors,
    type = type,
    n_colums = n_columns,
    ...
  )))
  invisible(x)
}
