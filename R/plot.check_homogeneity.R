#' Plot method for homogeneity of variances checks
#'
#' The `plot()` method for the `performance::check_homogeneity()`
#' function.
#'
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examples
#' library(performance)
#' model <<- lm(len ~ supp + dose, data = ToothGrowth)
#' result <- check_homogeneity(model)
#' result
#' plot(result)
#' @export
plot.see_check_homogeneity <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    model <- .retrieve_data(x)
  } else {
    model <- data
  }

  method <- paste0("Homogeneity of Variance (", attr(x, "method"), ")")

  dat <- insight::get_data(model)
  resp <- insight::get_response(model)
  pred <- insight::find_predictors(model, flatten = TRUE)

  if (length(pred) > 1) {
    l <- lapply(dat[, pred], as.character)
    for (i in pred[1:(length(pred) - 1)]) l[[i]] <- sprintf("%s \u00D7 ", l[[i]])
    x <- do.call(c, l)
    group_labels <- do.call(paste0, l)
    group <- rep(group_labels, each = length(pred))
    resp <- rep(resp, length(pred))
  } else {
    x <- as.character(as.vector(dat[, pred, drop = TRUE]))
    group <- 1
  }

  dat <- data.frame(
    x = x,
    y = resp,
    group = group,
    stringsAsFactors = FALSE
  )

  if (length(pred) > 1) {
    # group-mean-center response
    dat$y <- dat$y - stats::ave(
      dat[["y"]],
      dat[["group"]],
      FUN = mean, na.rm = TRUE
    )
    p <- ggplot(data = dat, aes(x = group, y = y, fill = group)) +
      geom_violin() +
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_label_repel(
          aes(label = group),
          y = 0, fill = "white",
          data = data.frame(group = unique(dat$group)),
          direction = "y",
          segment.colour = NA
        )
      } else {
        geom_label(
          aes(label = group),
          y = 0, fill = "white",
          data = data.frame(group = unique(dat$group))
        )
      }
  } else {
    # group-mean-center response
    dat$y <- dat$y - stats::ave(
      dat[["y"]],
      dat[["x"]],
      FUN = mean, na.rm = TRUE
    )
    p <- ggplot(data = dat, aes(x = x, y = y, fill = x)) +
      geom_violin() +
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_label_repel(
          aes(label = x),
          y = 0, fill = "white",
          data = data.frame(x = unique(dat$x)),
          direction = "y",
          segment.colour = NA
        )
      } else {
        geom_label(
          aes(label = x),
          y = 0, fill = "white",
          data = data.frame(x = unique(dat$x))
        )
      }
  }

  p +
    scale_fill_flat_d() +
    scale_x_discrete(labels = NULL) +
    theme_modern(
      base_size = 12,
      plot.title.space = 3,
      axis.title.space = 5
    ) +
    guides(fill = "none") +
    labs(
      x = NULL,
      y = paste0(insight::find_response(model), "\n(group mean-centered)"),
      title = method,
      subtitle = "Groups should be evenly spread"
    ) +
    theme(plot.title.position = "plot")
}
