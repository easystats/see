#' @export
print.see_check_model <- function(x,
                                  style = theme_lucid,
                                  colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                  ...) {
  plot(x, style = style, colors = colors, ...)
  invisible(x)
}
