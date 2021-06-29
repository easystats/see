#' Multiple plots side by side
#'
#' A wrapper around \emph{patchwork} to plot multiple figures side by side on the same page. See \href{https://patchwork.data-imaginist.com/articles/patchwork.html}{the \emph{patchwork} documentation} for more advanced control of plot layouts.
#'
#' @param n_rows Number of rows to align plots.
#' @param n_columns Number of columns to align plots.
#' @param guides 	A string specifying how guides should be treated in the layout. \code{'collect'} will collect shared guides across plots, removing duplicates. \code{'keep'} will keep guides alongside their plot. \code{'auto'} will inherit from a higher patchwork level (if any). See \code{\link[patchwork:plot_layout]{patchwork::plot_layout()}} for details.
#' @param tags Add tags to your subfigures. Can be \code{NULL} to omit (default) or a character vector containing tags for each plot. Automatic tags can also be generated with \code{'1'} for Arabic numerals, \code{'A'} for uppercase Latin letters, \code{'a'} for lowercase Latin letters, \code{'I'} for uppercase Roman numerals, and \code{'i'} for lowercase Roman numerals. For backwards compatibility, can also be \code{FALSE} (equivalent to \code{NULL}), \code{NA} (equivalent to \code{NULL}), or \code{TRUE} (equivalent to \code{'A'}).
#' @param tag_prefix,tag_suffix Text strings that should appear before or after the tag.
#' @param tag_sep Text string giving the separator to use between different tag levels.
#' @param title,subtitle,caption Text strings to use for the various plot annotations to add to the composed patchwork.
#' @param theme A ggplot theme specification to use for the plot. Only elements related to titles, caption, and tags, as well as plot margin and background, are used.
#' @examples
#' library(ggplot2)
#' library(see)
#'
#' p1 <- ggplot(mtcars, aes(x = disp, y = mpg)) +
#'   geom_point()
#' p2 <- ggplot(mtcars, aes(x = mpg)) +
#'   geom_density()
#' p3 <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'   geom_bar() +
#'   scale_x_discrete("cyl")
#'
#' plots(p1, p2)
#' plots(p1, p2, n_columns = 2, tags = 'A')
#' plots(
#'   p1, p2, p3,
#'   n_columns = 1, tags = c("Fig. 1", "Fig. 2", "Fig. 3"),
#'   title = "The surprising truth about mtcars"
#' )
#' @export
plots <- function(..., n_rows = NULL, n_columns = NULL, guides = NULL,
                  tags = FALSE, tag_prefix = NULL, tag_suffix = NULL, tag_sep = NULL,
                  title = NULL, subtitle = NULL, caption = NULL,
                  theme = NULL) {
  insight::check_if_installed("patchwork")

    # Add tags
  if (!is.null(tags)) {
    if (length(tags) == 1) {
      if (isTRUE(tags)) {
        tags <- 'A'
      } else if (isFALSE(tags) || is.na(tags)) {
        tags <- NULL
      } else if (! tags %in% c('1', 'A', 'a', 'I', 'i')) {
        tags <- list(tags)
      }
    } else {
      tags <- list(tags)
    }
  }

  pw <- patchwork::wrap_plots(..., nrow = n_rows, ncol = n_columns, guides = guides) +
    patchwork::plot_annotation(
      tag_levels = tags,
      tag_prefix = tag_prefix, tag_suffix = tag_suffix, tag_sep = tag_sep,
      title = title, subtitle = subtitle, caption = caption,
      theme = theme
    )
  return(pw)
}

