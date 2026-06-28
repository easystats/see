#' Plot group means
#'
#' @param x  A dw_groupmeans data frame.
#' @param y  Always `NULL` for this plot type
#' @param ... Arguments to be passed to methods. `title`
#' can be used to specify a string to use a title. `caption`
#' is a logical indicating if a caption summarizing the anova
#' results for the analysis should be included. By default this
#' is `TRUE`. `ci` is a logical indicating if the confidence
#' intervals from the `x` dw_groupmeans object should be included in the graph.
#'
#' @examples
#' group_means_object <-  datawizard::means_by_group(iris$Sepal.Width, iris$Species)
#' plot(group_means_object, title = "group means", ci = FALSE, caption = FALSE)
#'
#' @export

plot.dw_groupmeans <- function(
  x,
  y,
  ...
) {
  dotargs <- eval(substitute(alist(...)))
  dotargsnames <- names(dotargs)

  # set arguments to defaults if not specified.
  title <- ifelse("title" %in% dotargsnames, dotargs[["title"]], "")
  ci <- ifelse("ci" %in% dotargsnames, dotargs[["ci"]], TRUE)
  captionl <- ifelse("caption" %in% dotargsnames, dotargs[["caption"]], TRUE)

  caption <- ifelse(captionl == TRUE, build_caption(x), "")

  x |>
    datawizard::data_filter(x$Category != "Total") -> trimmed

  p <- ggplot2::ggplot(trimmed, aes(x = .data$Category, y = .data$Mean)) +
    geom_point() +
    labs(title = title, caption = caption)

  # There is an option not to return ci in data_group_means()
  if ("CI_low" %in% names(x) & ci != FALSE) {
    p <- p +
      geom_linerange(aes(ymin = .data$CI_low, ymax = .data$CI_high))
  }

  p
}


#' function to plot lists of `dw_groupmeans`
#'
#'
#' @param x  A dw_groupmeans data frame.
#' @param ... Other options. The function recognizes `title`,
#' `caption`, and `ci `. `title` is an overall title.
#'
#' @details
#' Produces a faceted plot when there is more than one
#' `dwtable`
#'
#'
#' @examples
#' group_means_object <-  datawizard::means_by_group(iris, Sepal.Width, Species)
#' plot(group_means_object, title = "group means", ci = FALSE, caption = FALSE)
#'
#'
#' @export
plot.dw_groupmeans_list <- function(
  x,
  y,
  ...
) {
  if (length(x) == 0L) {
    return("x is an empty list")
  }

  dotargs <- list(...)
  if (length(dotargs) == 0L) {
    dotargs <- list(title = "", caption = TRUE, ci = TRUE)
    dotargsnames <- names(dotargs)
  } else {
    dotargsnames <- names(dotargs)
    if (!"title" %in% dotargsnames) {
      dotargs[["title"]] <- ""
    }
    if (!"caption" %in% dotargsnames) {
      dotargs[["caption"]] <- TRUE
    } else {
      (dotargs[["caption"]] <- dotargs[["caption"]])
    }
    if (!"ci" %in% dotargsnames) {
      dotargs[["ci"]] <- TRUE
    }
  }
  # Need to update the names
  dotargsnames <- names(dotargs)

  title <- ifelse("title" %in% dotargsnames, dotargs[["title"]], "")
  ci <- ifelse("ci" %in% dotargsnames, dotargs[["ci"]], TRUE)
  caption <- ifelse("caption" %in% dotargsnames, dotargs[["caption"]], TRUE)
  if (length(x) == 1L) {
    p <- plot(
      x[[1]],
      title = title,
      ci = ci,
      caption = caption
    )
    return(p)
  }

  x_attributes <- lapply(x, attributes)
  x_captions <- lapply(x, build_caption)
  x_caption <- paste(x_captions, collapse = "")

  x_var_names <- lapply(x_attributes, `[[`, "var_mean_label")
  x_var_names <- as.character(x_var_names)

  x_long <- dplyr::bind_rows(x, .id = "origin_df")

  lengthx <- length(x)
  for (x_sub in 1:lengthx) {
    x_long$origin_df[x_long$origin_df == x_sub] <- x_var_names[x_sub]
  }

  trimmed <- datawizard::data_filter(x_long, Category != "Total")

  p <- ggplot(trimmed, aes(x = .data$Category, y = .data$Mean)) +
    geom_point() +
    facet_wrap(vars(origin_df))
  if (dotargs[["caption"]] == TRUE) {
    p <- p + labs(title = title, caption = x_caption)
  } else {
    p <- p + labs(title = title)
  }
  if ("CI_low" %in% names(trimmed) & dotargs["ci"] == TRUE) {
    p <- p +
      geom_linerange(aes(ymin = .data$CI_low, ymax = .data$CI_high))
  }

  p
}

build_caption <- function(x) {
  caption <- paste0(
    "\nAnova: R2=",
    insight::format_value(attributes(x)$r2, digits = 3),
    "; adj.R2=",
    insight::format_value(attributes(x)$adj.r2, digits = 3),
    "; F=",
    insight::format_value(attributes(x)$fstat, digits = 3),
    "; ",
    insight::format_p(attributes(x)$p.value, whitespace = FALSE),
    "\n"
  )
  caption
}
