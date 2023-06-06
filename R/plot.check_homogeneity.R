#' Plot method for homogeneity of variances checks
#'
#' The `plot()` method for the `performance::check_homogeneity()`
#' function.
#'
#' @inheritParams data_plot
#'
#' @return A ggplot2-object.
#'
#' @examplesIf require("performance")
#' model <<- lm(len ~ supp + dose, data = ToothGrowth)
#' result <- check_homogeneity(model)
#' result
#' plot(result)
#' @importFrom ggplot2 .data
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

  if (length(pred) > 1L) {
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

  if (length(pred) > 1L) {
    # group-mean-center response
    dat$y <- dat$y - stats::ave(
      dat[["y"]],
      dat[["group"]],
      FUN = mean, na.rm = TRUE
    )
    p <- ggplot(data = dat, aes(x = .data$group, y = .data$y, fill = .data$group)) +
      geom_violin() +
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_label_repel(
          aes(label = .data$group),
          y = 0, fill = "white",
          data = data.frame(
            group = unique(dat$group),
            stringsAsFactors = FALSE
          ),
          direction = "y",
          segment.colour = NA
        )
      } else {
        geom_label(
          aes(label = .data$group),
          y = 0, fill = "white",
          data = data.frame(
            group = unique(dat$group),
            stringsAsFactors = FALSE
          )
        )
      }
  } else {
    # group-mean-center response
    dat$y <- dat$y - stats::ave(
      dat[["y"]],
      dat[["x"]],
      FUN = mean, na.rm = TRUE
    )
    p <- ggplot(data = dat, aes(x = .data$x, y = .data$y, fill = .data$x)) +
      geom_violin() +
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        ggrepel::geom_label_repel(
          aes(label = .data$x),
          y = 0, fill = "white",
          data = data.frame(
            x = unique(dat$x),
            stringsAsFactors = FALSE
          ),
          direction = "y",
          segment.colour = NA
        )
      } else {
        geom_label(
          aes(label = .data$x),
          y = 0, fill = "white",
          data = data.frame(
            x = unique(dat$x),
            stringsAsFactors = FALSE
          )
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


.plot_diag_homogeneity <- function(x,
                                   size_point,
                                   size_line,
                                   alpha_level = 0.2,
                                   theme_style = theme_lucid,
                                   colors = unname(social_colors(c("green", "blue", "red"))),
                                   dot_alpha_level = 0.8,
                                   show_dots = TRUE) {
  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data$x, .data$y))

  if (isTRUE(show_dots)) {
    p <- p +
      geom_point2(
        colour = colors[2],
        size = size_point,
        alpha = dot_alpha_level
      )
  }

  p +
    ggplot2::stat_smooth(
      method = "loess",
      se = TRUE,
      alpha = alpha_level,
      formula = y ~ x,
      linewidth = size_line,
      colour = colors[1]
    ) +
    ggplot2::labs(
      title = "Homogeneity of Variance",
      subtitle = "Reference line should be flat and horizontal",
      y = expression(sqrt("|Std. residuals|")),
      x = "Fitted values"
    ) +
    theme_style(
      base_size = 10,
      plot.title.space = 3,
      axis.title.space = 5
    )
}
