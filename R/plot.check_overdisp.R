#' @export
plot.see_check_overdisp <- function(x,
                                    size_line = 0.8,
                                    colors = c("#3aaf85", "#1b6ca8"),
                                    type = 1,
                                    ...) {
  .plot_diag_overdispersion(
    x,
    style = theme_lucid,
    colors = colors,
    size_line = size_line,
    type = type,
  )
}
