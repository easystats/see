#' @export
print.see_check_model <- function(x,
                                  style = theme_lucid,
                                  colors = NULL,
                                  ...) {
  suppressWarnings(suppressMessages(print(plot(x, style = style, colors = colors, ...))))
  invisible(x)
}
