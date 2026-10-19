#' Lighthouse beams
#'
#' Draw the beams of a lighthouse plot. Each row of the data draws one beam: a
#' filled triangle from the point (`x`, `y`) to the interval from `ymin` to
#' `ymax` at `xend`. In a lighthouse plot of contrasts, as drawn by `plot()`
#' for `modelbased::estimate_contrasts()`, `x` and `y` are a level and its
#' estimated mean, and the interval at `xend` holds the values of the other
#' level's mean that the confidence interval of the contrast allows.
#'
#' The three corners are drawn in the order (`x`, `y`), (`xend`, `ymin`),
#' (`xend`, `ymax`), and `ymin` need not be smaller than `ymax`. The beams are
#' drawn in the order of the rows, so later rows are on top.
#'
#' On a discrete x axis, the axis takes the levels of `x` first and then the
#' levels that only `xend` holds, so set the order with
#' `scale_x_discrete(limits = ...)` where it matters. `position_dodge()` moves
#' `x` only, so it moves the start of a beam but not its interval.
#' `coord_flip()` draws the beams horizontally. The beams are translucent by
#' default (`alpha = 0.1`), and so are their keys in a fill legend; use
#' `guides(fill = guide_legend(override.aes = list(alpha = 1)))` to draw the
#' keys opaque.
#'
#' @section Aesthetics:
#' `geom_lightbeam()` understands the following aesthetics (required ones are
#' in bold): **`x`**, **`y`**, **`xend`**, **`ymin`**, **`ymax`**, `alpha`,
#' `colour`, `fill`, `group`, `linetype`, and `linewidth`. Rows with a missing
#' value in a required aesthetic are not drawn, with a warning.
#'
#' @inheritParams ggplot2::geom_polygon
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' library(ggplot2)
#'
#' # estimated means of three groups, with their 95% confidence intervals
#' means <- data.frame(
#'   group = c("A", "B", "C"),
#'   mean = c(2, 3, 5),
#'   low = c(1.5, 2.4, 4.3),
#'   high = c(2.5, 3.6, 5.7)
#' )
#'
#' # pairwise differences (Level1 minus Level2), with their 95% intervals
#' contrasts <- data.frame(
#'   Level1 = c("B", "C", "C"),
#'   Level2 = c("A", "A", "B"),
#'   CI_low = c(0.2, 2.1, 1.1),
#'   CI_high = c(1.8, 3.9, 2.9)
#' )
#'
#' # each beam starts at the mean of Level1 and spans the values of the mean
#' # of Level2 that the interval of the difference allows
#' contrasts$Mean1 <- means$mean[match(contrasts$Level1, means$group)]
#' contrasts$tip_low <- contrasts$Mean1 - contrasts$CI_low
#' contrasts$tip_high <- contrasts$Mean1 - contrasts$CI_high
#'
#' ggplot() +
#'   geom_lightbeam(
#'     data = contrasts,
#'     aes(x = Level1, y = Mean1, xend = Level2, ymin = tip_low, ymax = tip_high)
#'   ) +
#'   geom_pointrange(
#'     data = means,
#'     aes(x = group, y = mean, ymin = low, ymax = high)
#'   ) +
#'   scale_x_discrete(limits = c("A", "B", "C")) +
#'   labs(x = "Group", y = "Mean")
#'
#' # a horizontal lighthouse
#' ggplot() +
#'   geom_lightbeam(
#'     data = contrasts,
#'     aes(x = Level1, y = Mean1, xend = Level2, ymin = tip_low, ymax = tip_high)
#'   ) +
#'   geom_pointrange(
#'     data = means,
#'     aes(x = group, y = mean, ymin = low, ymax = high)
#'   ) +
#'   scale_x_discrete(limits = c("A", "B", "C")) +
#'   labs(x = "Group", y = "Mean") +
#'   coord_flip()
#'
#' @export
geom_lightbeam <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLightbeam,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @rdname geom_lightbeam
#' @format NULL
#' @usage NULL
#' @export
GeomLightbeam <- ggplot2::ggproto(
  "GeomLightbeam",
  ggplot2::Geom,
  required_aes = c("x", "y", "xend", "ymin", "ymax"),
  # the defaults of geom_polygon(), whose expressions are evaluated in the
  # namespace of ggplot2, with the beams translucent
  default_aes = local({
    default_aes <- ggplot2::GeomPolygon$default_aes
    default_aes$subgroup <- NULL
    default_aes$alpha <- 0.1
    default_aes
  }),
  draw_key = ggplot2::draw_key_polygon,
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10
  ) {
    # one polygon per row, with the corners (x, y), (xend, ymin), (xend, ymax)
    # in this order, drawn in the order of the rows
    n <- nrow(data)
    vertices <- data[rep(seq_len(n), each = 3), , drop = FALSE]
    vertices$x <- as.vector(rbind(data$x, data$xend, data$xend))
    vertices$y <- as.vector(rbind(data$y, data$ymin, data$ymax))
    vertices$group <- rep(seq_len(n), each = 3)
    vertices$xend <- vertices$ymin <- vertices$ymax <- NULL
    ggplot2::GeomPolygon$draw_panel(
      vertices,
      panel_params,
      coord,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre
    )
  }
)
