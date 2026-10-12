skip_if_not_installed("ggdist")

iris_long <- data.frame(
  Species = rep(iris$Species, times = 4),
  variable = rep(names(iris)[1:4], each = nrow(iris)),
  values = unlist(iris[1:4], use.names = FALSE)
)

# the dots layer is the one built on ggdist's GeomDots
dots_layer <- function(layers) {
  is_dots <- vapply(
    layers,
    function(l) inherits(l$geom, "GeomDots"),
    logical(1)
  )
  layers[[which(is_dots)]]
}

test_that("geom_violindot() respects free scales in facet_wrap()", {
  base <- ggplot2::ggplot(iris_long, ggplot2::aes(Species, values)) +
    ggplot2::facet_wrap(~variable, scales = "free_y")
  y_ranges <- function(p) {
    lapply(ggplot2::ggplot_build(p)$layout$panel_params, `[[`, "y.range")
  }

  expect_equal(
    y_ranges(base + geom_violindot()),
    y_ranges(base + ggplot2::geom_violin()),
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violindot() lets ggdist choose the bin width", {
  expect_identical(dots_layer(geom_violindot())$geom_params$binwidth, NA)
  expect_identical(
    dots_layer(geom_violindot(binwidth = 0.1))$geom_params$binwidth,
    0.1
  )
})

test_that("geom_violindot() draws dots on the left and the slab on the right", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot()
  b <- ggplot2::ggplot_build(p)
  is_dots <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomDots"),
    logical(1)
  )

  expect_identical(unique(b$data[[which(is_dots)]]$side), "left")
  expect_identical(unique(b$data[[which(!is_dots)]]$side), "right")
})

test_that("geom_violindot() passes interval on", {
  expect_length(geom_violindot(interval = TRUE), length(geom_violindot()) + 1L)
})

test_that("geom_violindot() keeps its old dot arguments working", {
  expect_identical(
    dots_layer(geom_violindot(dots_size = 1.2))$geom_params$dotsize,
    1.2
  )
  expect_identical(
    dots_layer(geom_violindot(size_dots = 1.2))$geom_params$dotsize,
    1.2
  )

  built_dots <- function(...) {
    p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
      geom_violindot(...)
    is_dots <- vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomDots"),
      logical(1)
    )
    ggplot2::ggplot_build(p)$data[[which(is_dots)]]
  }
  expect_identical(unique(built_dots(dots_color = "red")$colour), "red")
  expect_identical(unique(built_dots(color_dots = "red")$colour), "red")
  expect_identical(unique(built_dots(dots_fill = "blue")$fill), "blue")
  expect_identical(unique(built_dots(fill_dots = "blue")$fill), "blue")

  nudged <- dots_layer(
    geom_violindot(position_dots = ggplot2::position_nudge(x = -0.1))
  )
  expect_s3_class(nudged$position, "PositionNudge")
  expect_identical(nudged$position$x, -0.1)
})
