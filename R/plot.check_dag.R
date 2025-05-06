#' Plot method for check DAGs
#'
#' The `plot()` method for the `performance::check_dag()` function.
#'
#' @param x A `check_dag` object.
#' @param size_point Numeric value specifying size of point geoms.
#' @param size_text Numeric value specifying size of text elements.
#' @param colors Character vector of length five, indicating the colors (in
#' hex-format) for different types of variables, which are assigned in following
#' order: `outcome`, `exposure`, `adjusted`, `unadjusted`, and `collider`.
#' @param which Character string indicating which plot to show. Can be either
#' `"all"`, `"current"` or `"required"`.
#' @param check_colliders Logical indicating whether to highlight colliders.
#' Set to `FALSE` if the algorithm to detect colliders is very slow.
#' @param effect Character string indicating which effect for the required model
#' is to be estimated. Can be either `"total"` or `"direct"`.
#' @param ... Currently not used.
#'
#' @return A ggplot2-object.
#'
#' @examplesIf require("ggdag", quietly = TRUE)
#' library(performance)
#' # incorrect adjustment
#' dag <- check_dag(
#'   y ~ x + b + c,
#'   x ~ b,
#'   outcome = "y",
#'   exposure = "x"
#' )
#' dag
#' plot(dag)
#'
#' # plot only model with required adjustments
#' plot(dag, which = "required")
#'
#' # collider-bias?
#' dag <- check_dag(
#'   y ~ x + c + d,
#'   x ~ c + d,
#'   b ~ x,
#'   b ~ y,
#'   outcome = "y",
#'   exposure = "x",
#'   adjusted = "c"
#' )
#' plot(dag)
#'
#' # longer labels, automatic detection of outcome and exposure
#' dag <- check_dag(
#'   QoL ~ age + education + gender,
#'   age ~ education
#' )
#' plot(dag)
#' @export
plot.see_check_dag <- function(
  x,
  size_point = 20,
  size_text = 4.5,
  colors = NULL,
  which = "all",
  effect = "total",
  check_colliders = TRUE,
  ...
) {
  .data <- NULL
  insight::check_if_installed(c("ggdag", "ggplot2"))
  which <- insight::validate_argument(which, c("all", "current", "required"))
  effect <- insight::validate_argument(effect, c("total", "direct"))

  # get plot data
  p1 <- p2 <- suppressWarnings(ggdag::dag_adjustment_sets(x, effect = effect))
  adjusted_for <- attributes(x)$adjusted

  # if we have multiple sets, we only need one for the current model
  if (!is.null(p1$data$set) && insight::n_unique(p1$data$set) > 1) {
    p1$data <- p1$data[p1$data$set == p1$data$set[1], ]
    # rename set-variables
    p2$data$set <- gsub("\\{(.*)\\}", "\\1", p2$data$set)
    p2$data$set <- paste0("Adjusted for ", p2$data$set)
  }

  # for current plot, we need to update the "adjusted" column
  p1$data$adjusted <- "unadjusted"
  if (!is.null(adjusted_for)) {
    p1$data$adjusted[p1$data$name %in% adjusted_for] <- "adjusted"
  }

  # tweak data
  p1$data$type <- as.character(p1$data$adjusted)
  if (check_colliders) {
    p1$data$type[vapply(
      p1$data$name,
      ggdag::is_collider,
      logical(1),
      .dag = x
    )] <- "collider"
  }
  p1$data$type[p1$data$name == attributes(x)$outcome] <- "outcome"
  p1$data$type[p1$data$name %in% attributes(x)$exposure] <- "exposure"
  p1$data$type <- factor(
    p1$data$type,
    levels = c("outcome", "exposure", "adjusted", "unadjusted", "collider")
  )

  p2$data$type <- as.character(p2$data$adjusted)
  if (check_colliders) {
    p2$data$type[vapply(
      p2$data$name,
      ggdag::is_collider,
      logical(1),
      .dag = x
    )] <- "collider"
  }
  p2$data$type[p2$data$name == attributes(x)$outcome] <- "outcome"
  p2$data$type[p2$data$name %in% attributes(x)$exposure] <- "exposure"
  p2$data$type <- factor(
    p2$data$type,
    levels = c("outcome", "exposure", "adjusted", "unadjusted", "collider")
  )

  if (is.null(colors)) {
    point_colors <- see_colors(c(
      "yellow",
      "cyan",
      "blue grey",
      "red",
      "orange"
    ))
  } else if (length(colors) != 5) {
    insight::format_error(
      "`colors` must be a character vector with five color-values."
    )
  } else {
    point_colors <- colors
  }
  names(point_colors) <- c(
    "outcome",
    "exposure",
    "adjusted",
    "unadjusted",
    "collider"
  )

  # these geoms are shared by both plots
  common_layers <- list(
    geom_point_borderless(
      ggplot2::aes(fill = .data$type),
      size = size_point
    ),
    ggdag::geom_dag_edges(
      ggplot2::aes(
        xend = .data$xend,
        yend = .data$yend,
        edge_alpha = .data$adjusted
      )
    ),
    ggdag::scale_adjusted(),
    ggdag::geom_dag_label(
      ggplot2::aes(label = .data$name),
      size = size_text
    ),
    ggdag::theme_dag(
      legend.text = ggplot2::element_text(size = 2.75 * size_text),
      legend.position = "bottom",
      legend.justification = c(0, 1)
    ),
    ggplot2::scale_fill_manual(values = point_colors),
    ggplot2::guides(
      edge_alpha = "none",
      fill = ggplot2::guide_legend(override.aes = list(size = size_point / 4))
    ),
    ggdag::expand_plot(
      expand_x = ggplot2::expansion(c(0.2, 0.2)),
      expand_y = ggplot2::expansion(c(0.2, 0.2))
    ),
    ggplot2::labs(fill = NULL)
  )

  # plot1 - current model
  plot1 <- ggplot2::ggplot(p1$data, ggplot2::aes(x = .data$x, y = .data$y)) +
    common_layers +
    ggplot2::ggtitle("Current model")

  # plot2 - required model
  plot2 <- ggplot2::ggplot(p2$data, ggplot2::aes(x = .data$x, y = .data$y)) +
    common_layers +
    ggplot2::ggtitle(sprintf("Required model (%s effect)", effect))

  # if we have multiple sets, we want to facet the second plot by sets
  if (!is.null(p2$data$set) && insight::n_unique(p2$data$set) > 1) {
    plot2 <- plot2 +
      ggplot2::facet_wrap(
        ~set,
        scales = "free",
        ncol = ceiling(sqrt(insight::n_unique(p2$data$set)))
      )
  }

  if (which == "all") {
    # fix legends - remove the legend that has fewer items, so all items
    # in the legend are shown for the integrated plot
    if (insight::n_unique(p1$data$type) > insight::n_unique(p2$data$type)) {
      plot2 <- plot2 + ggplot2::theme(legend.position = "none")
    } else {
      plot1 <- plot1 + ggplot2::theme(legend.position = "none")
    }
    # plot
    plots(plot1, plot2, n_rows = 1)
  } else if (which == "current") {
    plot1
  } else {
    plot2
  }
}
