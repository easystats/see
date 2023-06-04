#' @export
print.see_check_model <- function(x,
                                  style = theme_lucid,
                                  colors = NULL,
                                  type = "density",
                                  n_columns = 2,
                                  ...) {
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
