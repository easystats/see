iris_long <- data.frame(
  Species = rep(iris$Species, times = 4),
  variable = rep(names(iris)[1:4], each = nrow(iris)),
  values = unlist(iris[1:4], use.names = FALSE)
)

test_that("geom_violinhalf() respects free scales in facet_wrap()", {
  p <- ggplot2::ggplot(iris_long, ggplot2::aes(Species, values)) +
    geom_violinhalf() +
    ggplot2::facet_wrap(~variable, scales = "free_y")
  ref <- ggplot2::ggplot(iris_long, ggplot2::aes(Species, values)) +
    ggplot2::geom_violin() +
    ggplot2::facet_wrap(~variable, scales = "free_y")

  y_ranges <- function(plot) {
    lapply(ggplot2::ggplot_build(plot)$layout$panel_params, `[[`, "y.range")
  }
  expect_identical(y_ranges(p), y_ranges(ref))
})

test_that("geom_violinhalf() computes bounds per panel in facet_grid()", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    geom_violinhalf() +
    ggplot2::facet_grid(am ~ vs)
  ld <- suppressWarnings(ggplot2::layer_data(p))

  by_cell <- split(ld, interaction(ld$PANEL, ld$group, drop = TRUE))
  for (cell in by_cell) {
    expect_identical(unique(cell$ymin), min(cell$y))
    expect_identical(unique(cell$ymax), max(cell$y))
  }
})

test_that("geom_violindot() respects free scales in facet_wrap()", {
  p <- ggplot2::ggplot(iris_long, ggplot2::aes(Species, values)) +
    geom_violindot() +
    ggplot2::facet_wrap(~variable, scales = "free_y")
  ranges <- lapply(
    ggplot2::ggplot_build(p)$layout$panel_params,
    `[[`,
    "y.range"
  )
  expect_length(unique(ranges), 4)
})
