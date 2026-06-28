#' Plot method for grouped means.
#'
#' The `plot()` method for the `datawizard::means_by_group()` function.
#'
#' @param x  An object returned `datawizard::means_by_group()`.
#' @param title String, can be used to specify a plot title.
#' @param caption Logical, indicating if a caption summarizing the anova results
#' for the analysis should be included.
#' @param ci Logical, indicating if the confidence intervals should be included
#' in the graph.
#' @param ... Currently not used.
#'
#' @details
#' Produces a faceted plot when there is more than one means-table.
#'
#' @examples
#' group_means_object <-  datawizard::means_by_group(iris$Sepal.Width, iris$Species)
#' plot(group_means_object, title = "group means", ci = FALSE, caption = FALSE)
#'
#' group_means_object <- datawizard::means_by_group(
#'   iris,
#'   c("Sepal.Width", "Petal.Width"),
#'   "Species"
#' )
#' plot(group_means_object, title = "group means")
#' @export
plot.see_dw_groupmeans <- function(
  x,
  title = "",
  ci = TRUE,
  caption = TRUE,
  ...
) {
  .data <- NULL
  caption_text <- if (isTRUE(caption)) .build_caption(x) else ""

  trimmed <- x[x$Category != "Total", ]

  p <- ggplot2::ggplot(
    trimmed,
    ggplot2::aes(x = .data$Category, y = .data$Mean)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(title = title, caption = caption_text)

  # There is an option not to return ci in data_group_means()
  if ("CI_low" %in% names(x) && isTRUE(ci)) {
    p <- p +
      ggplot2::geom_linerange(ggplot2::aes(
        ymin = .data$CI_low,
        ymax = .data$CI_high
      ))
  }

  p + ggplot2::coord_flip()
}


#' @export
plot.see_dw_groupmeans_list <- function(
  x,
  title = "",
  ci = TRUE,
  caption = TRUE,
  ...
) {
  .data <- NULL

  if (length(x) == 1L) {
    p <- plot(
      x[[1]],
      title = title,
      ci = ci,
      caption = caption,
      ...
    )
    return(p)
  }

  if (!length(x)) {
    insight::format_error("`x` is an empty object.")
  }

  x_attributes <- lapply(x, attributes)
  x_var_names <- lapply(x_attributes, `[[`, "var_mean_label")
  x_captions <- mapply(.build_caption, x, x_var_names)
  x_caption <- paste(x_captions, collapse = "")

  x <- lapply(seq_along(x_var_names), function(i) {
    grp_name <- x_var_names[[i]]
    if (is.null(grp_name)) {
      grp_name <- paste0("Group ", i, ":")
    }
    x[[i]]$origin_df <- grp_name
    x[[i]]
  })

  x_long <- do.call(rbind, x)
  trimmed <- trimmed <- x_long[x_long$Category != "Total", ]

  p <- ggplot2::ggplot(
    trimmed,
    ggplot2::aes(x = .data$Category, y = .data$Mean)
  ) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~origin_df)

  if (isTRUE(caption)) {
    p <- p + ggplot2::labs(title = title, caption = x_caption)
  } else {
    p <- p + ggplot2::labs(title = title)
  }
  if ("CI_low" %in% names(trimmed) && isTRUE(ci)) {
    p <- p +
      ggplot2::geom_linerange(ggplot2::aes(
        ymin = .data$CI_low,
        ymax = .data$CI_high
      ))
  }

  p + ggplot2::coord_flip()
}


.build_caption <- function(x, label = NULL) {
  if (is.null(label)) {
    label <- ""
  } else {
    label <- paste0(label, ": ")
  }

  caption <- paste0(
    "\n",
    label,
    "Anova: R2=",
    insight::format_value(attributes(x)$r2, digits = 3),
    "; adj.R2=",
    insight::format_value(attributes(x)$adj.r2, digits = 3),
    "; F=",
    insight::format_value(attributes(x)$fstat, digits = 3),
    "; ",
    insight::format_p(attributes(x)$p.value, whitespace = FALSE)
  )

  caption
}
