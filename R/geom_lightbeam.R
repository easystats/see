#' Lighthouse beams
#'
#' Draws the beams of a lighthouse plot, as in `plot()` for
#' `modelbased::estimate_contrasts()`. Each row draws a filled triangle from
#' (`x`, `y`) to the interval from `ymin` to `ymax` at `xend`. Use
#' `coord_flip()` for horizontal beams.
#'
#' @section Aesthetics:
#' `geom_lightbeam()` understands the following aesthetics (required ones are
#' in bold): **`x`**, **`y`**, **`xend`**, **`ymin`**, **`ymax`**, `alpha`,
#' `colour`, `fill`, `group`, `linetype`, and `linewidth`.
#'
#' @inheritParams ggplot2::geom_polygon
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' library(ggplot2)
#'
#' means <- data.frame(
#'   group = c("A", "B", "C"),
#'   mean = c(2, 3, 5),
#'   low = c(1.5, 2.4, 4.3),
#'   high = c(2.5, 3.6, 5.7)
#' )
#'
#' # differences Level1 - Level2, with their 95% intervals
#' contrasts <- data.frame(
#'   Level1 = c("B", "C", "C"),
#'   Level2 = c("A", "A", "B"),
#'   CI_low = c(0.2, 2.1, 1.1),
#'   CI_high = c(1.8, 3.9, 2.9)
#' )
#' contrasts$Mean1 <- means$mean[match(contrasts$Level1, means$group)]
#' contrasts$tip_low <- contrasts$Mean1 - contrasts$CI_low
#' contrasts$tip_high <- contrasts$Mean1 - contrasts$CI_high
#'
#' p <- ggplot() +
#'   geom_lightbeam(
#'     data = contrasts,
#'     aes(x = Level1, y = Mean1, xend = Level2, ymin = tip_low, ymax = tip_high)
#'   ) +
#'   geom_pointrange(
#'     data = means,
#'     aes(x = group, y = mean, ymin = low, ymax = high)
#'   ) +
#'   scale_x_discrete(limits = c("A", "B", "C"))
#' p
#'
#' # horizontal beams
#' p + coord_flip()
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
  # defaults of geom_polygon(), but translucent
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
    # one triangle per row: (x, y), (xend, ymin), (xend, ymax)
    n <- nrow(data)
    vertices <- data[rep(seq_len(n), each = 3), , drop = FALSE]
    vertices$x <- as.vector(rbind(data$x, data$xend, data$xend))
    vertices$y <- as.vector(rbind(data$y, data$ymin, data$ymax))
    vertices$group <- rep(seq_len(n), each = 3)
    # beams have no holes, so drop `subgroup`
    vertices$xend <- vertices$ymin <- vertices$ymax <- vertices$subgroup <- NULL
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
