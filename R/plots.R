#' Multiple plots side by side
#'
#' A wrapper around *patchwork* to plot multiple figures side by side on
#' the same page.
#'
#' See [the *patchwork* documentation](https://patchwork.data-imaginist.com/articles/patchwork.html)
#' for more advanced control of plot layouts.
#'
#' @param ... Multiple `ggplot`s or a list containing `ggplot` objects
#' @param n_rows Number of rows to align plots.
#' @param n_columns Number of columns to align plots.
#' @param guides A string specifying how guides should be treated in the
#'   layout. `'collect'` will collect shared guides across plots, removing
#'   duplicates. `'keep'` will keep guides alongside their plot.
#'   `'auto'` will inherit from a higher patchwork level (if any). See
#'   [patchwork::plot_layout()] for details.
#' @param tags Add tags to your subfigures. Can be `NULL` to omit (default)
#'   or a character vector containing tags for each plot. Automatic tags can
#'   also be generated with `'1'` for Arabic numerals, `'A'` for
#'   uppercase Latin letters, `'a'` for lowercase Latin letters, `'I'`
#'   for uppercase Roman numerals, and `'i'` for lowercase Roman numerals.
#'   For backwards compatibility, can also be `FALSE` (equivalent to
#'   `NULL`), `NA` (equivalent to `NULL`), or `TRUE`
#'   (equivalent to `'A'`).
#' @param tag_prefix,tag_suffix Text strings that should appear before or after
#'   the tag.
#' @param tag_sep Text string giving the separator to use between different tag
#'   levels.
#' @param title,subtitle,caption Text strings to use for the various plot
#'   annotations to add to the composed patchwork.
#' @param theme A ggplot theme specification to use for the plot. Only elements
#'   related to titles, caption, and tags, as well as plot margin and
#'   background, are used.
#'
#' @examplesIf require("patchwork", quietly = TRUE)
#' library(ggplot2)
#' library(see)
#'
#' p1 <- ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   geom_point()
#' p2 <- ggplot(mtcars, aes(x = mpg)) +
#'   geom_density()
#' p3 <- ggplot(mtcars, aes(x = factor(cyl))) +
#'   geom_bar() +
#'   scale_x_discrete("cyl")
#'
#' plots(p1, p2)
#' plots(p1, p2, n_columns = 2, tags = "A")
#' plots(
#'   p1, p2, p3,
#'   n_columns = 1, tags = c("Fig. 1", "Fig. 2", "Fig. 3"),
#'   title = "The surprising truth about mtcars"
#' )
#' @export
plots <- function(
  ...,
  n_rows = NULL,
  n_columns = NULL,
  guides = NULL,
  tags = FALSE,
  tag_prefix = NULL,
  tag_suffix = NULL,
  tag_sep = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  theme = NULL
) {
  # Add tags
  if (!is.null(tags)) {
    if (length(tags) == 1L) {
      if (isTRUE(tags)) {
        tags <- "A"
      } else if (isFALSE(tags) || is.na(tags)) {
        tags <- NULL
      } else if (!tags %in% c("1", "A", "a", "I", "i")) {
        tags <- list(tags)
      }
    } else {
      tags <- list(tags)
    }
  }

  patchwork::wrap_plots(..., nrow = n_rows, ncol = n_columns, guides = guides) +
    patchwork::plot_annotation(
      tag_levels = tags,
      tag_prefix = tag_prefix,
      tag_suffix = tag_suffix,
      tag_sep = tag_sep,
      title = title,
      subtitle = subtitle,
      caption = caption,
      theme = theme
    )
}


.safe_print_plots <- function(pw, ...) {
  pw_drawn <- tryCatch(print(pw), error = function(e) e)
  # for debugging, return original error
  if (isTRUE(getOption("easystats_errors", FALSE))) {
    insight::format_error(pw_drawn$message)
  }
  if (inherits(pw_drawn, "simpleError")) {
    msg_display1 <- "\n- To fix this issue, please make the window larger."
    msg_display3 <- "\n- If this still doesn't resolve your problems, you may check whether your apps are rescaled. On Windows, this can be done in the display settings (Start > Settings > System > Display, \"Scale and layout\"). Reduce the scaling and try again." # nolint
    msg_display4 <- "\n- You can try to decrease the base font-size of your theme before plotting. Load `library(ggplot2)` and run: `theme_set(theme_classic(base_size = 6))`" # nolint
    msg_display5 <- "\n- Finally, make sure to update your packages (in particular `ggplot2` and `patchwork`) using `update.packages(ask = FALSE)`." # nolint

    if (Sys.getenv("RSTUDIO") == "1") {
      msg <- "The RStudio 'Plots' window is too small to show this set of plots."
      msg_display2 <- "\n- If this doesn't help, try to reset your zoom settings. In RStudio, go to Menu \"View > Actual Size\" and then retry." # nolint
    } else {
      msg <- "The viewport is too small to show this set of plots."
      msg_display2 <- "\n- If this doesn't help, try to reset the zoom settings of your IDE and then retry."
    }

    msg <- paste(
      msg,
      "You may try one of the following steps to resolve this problem."
    )
    insight::format_error(
      msg,
      msg_display1,
      msg_display2,
      msg_display3,
      msg_display4,
      msg_display5
    )
  }
}
