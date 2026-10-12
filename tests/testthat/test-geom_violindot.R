skip_if_not_installed("ggdist", "3.3.3")

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

  nudge <- ggplot2::position_nudge(x = -0.1)
  expect_identical(
    dots_layer(geom_violindot(position_dots = nudge))$position,
    nudge
  )
})

test_that("geom_violindot() dot arguments win over the same names in ...", {
  built <- function(...) {
    p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
      geom_violindot(...)
    is_dots <- vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomDots"),
      logical(1)
    )
    b <- ggplot2::ggplot_build(p)
    list(dots = b$data[[which(is_dots)]], slab = b$data[[which(!is_dots)]])
  }

  fills <- built(fill = "red", fill_dots = "blue")
  expect_identical(unique(fills$dots$fill), "blue")
  expect_identical(unique(fills$slab$fill), "red")

  expect_no_warning(built(color = "red", color_dots = "blue"))
  cols <- built(color = "red", color_dots = "blue")
  expect_identical(unique(cols$dots$colour), "blue")
  expect_identical(
    unique(built(colour = "red", color_dots = "blue")$dots$colour),
    "blue"
  )
})

test_that("geom_violindot(flip = TRUE) gives no warning from the dots", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(flip = TRUE)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("geom_violindot(interval = TRUE) moves the dots clear of the interval", {
  expect_identical(dots_layer(geom_violindot())$position$x, -0.025)
  expect_identical(
    dots_layer(geom_violindot(interval = TRUE))$position$x,
    -0.06
  )
  nudge <- ggplot2::position_nudge(x = -0.2)
  expect_identical(
    dots_layer(geom_violindot(interval = TRUE, position_dots = nudge))$position,
    nudge
  )
})

test_that("geom_violindot() keeps half-violin arguments away from the dots", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(n = 100, bounds = c(4, 8), .width = 0.5)
  expect_no_warning(ggplot2::ggplot_build(p))

  both <- geom_violindot(side = "both")
  expect_identical(dots_layer(both)$aes_params$side, "left")
})

test_that("geom_violindot() dot arguments win over base-graphics aliases", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(
      col = "red",
      color_dots = "blue",
      bg = "red",
      fill_dots = "green"
    )
  expect_no_warning(ggplot2::ggplot_build(p))
  is_dots <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomDots"),
    logical(1)
  )
  dots <- ggplot2::layer_data(p, which(is_dots))
  expect_identical(unique(dots$colour), "blue")
  expect_identical(unique(dots$fill), "green")
})

test_that("geom_violindot() gives the dots the same space as the half violins", {
  set.seed(1)
  d <- data.frame(x = rep(c(10, 20, 30), each = 20), y = stats::rnorm(60))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, group = x)) + geom_violindot()
  is_dots <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomDots"),
    logical(1)
  )
  dots <- ggplot2::layer_data(p, which(is_dots))
  expect_equal(
    unique(round(dots$xmax - dots$xmin, 6)),
    4.5,
    tolerance = testthat::testthat_tolerance()
  )

  p2 <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(width = 0.6)
  dots2 <- ggplot2::layer_data(p2, 2)
  slab2 <- ggplot2::layer_data(p2, 1)
  expect_equal(
    unique(round(dots2$xmax - dots2$xmin, 6)),
    0.3,
    tolerance = testthat::testthat_tolerance()
  )
  expect_equal(
    unique(round(slab2$xmax - slab2$xmin, 6)),
    0.3,
    tolerance = testthat::testthat_tolerance()
  )
})

test_that("geom_violindot() keeps slab-only arguments away from the dots", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    geom_violindot(expand = TRUE, outline_bars = FALSE, show_point = TRUE)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("geom_violindot() draws dodged dots in the free half of their cell", {
  p <- ggplot2::ggplot(
    iris_ab,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
  ) +
    geom_violindot()
  m <- dots_measure(p)

  expect_setequal(unique(m$group), 1:6)
  # each dot centre within [xmin, x] of its group's cell, within 1e-6
  expect_gt(min(m$dot_x - m$pos_min), -1e-6)
  expect_lt(max(m$dot_x - m$pos), 1e-6)
  expect_extents(slab_measure(p))
})

test_that("geom_violindot() keeps dodged dots in narrow cells", {
  d <- iris
  d$g <- factor(rep(1:4, length.out = nrow(d)))
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
  ) +
    geom_violindot(interval = TRUE)
  expect_no_warning(m <- dots_measure(p))

  expect_setequal(unique(m$group), 1:12)
  expect_gt(min(m$dot_x - m$pos_min), -1e-6)
  expect_lt(max(m$dot_x - m$pos), 1e-6)
})

test_that("geom_violindot() keeps the dot nudge for one group per position", {
  built_x <- function(...) {
    p <- ggplot2::ggplot(
      iris,
      ggplot2::aes(Species, Sepal.Length, fill = Species)
    ) +
      geom_violindot(...)
    is_dots <- vapply(
      p$layers,
      function(l) inherits(l$geom, "GeomDots"),
      logical(1)
    )
    sort(unique(as.numeric(ggplot2::layer_data(p, which(is_dots))$x)))
  }
  expect_equal(built_x(), c(0.975, 1.975, 2.975))
  expect_equal(built_x(interval = TRUE), c(0.94, 1.94, 2.94))
})

test_that("geom_violindot() uses a user position_dots as given under dodge", {
  nudge <- ggplot2::position_nudge(x = -0.1)
  p <- ggplot2::ggplot(
    iris_ab,
    ggplot2::aes(Species, Sepal.Length, fill = interaction(g, Species))
  ) +
    geom_violindot(position_dots = nudge)
  expect_identical(dots_layer(p$layers)$position, nudge)
})
