skip_if_not_installed("ggdist", "3.3.3")

vertical <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length))
horizontal <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Species))

# the layers of a raincloud without shading: slab, interval, points
raincloud_layers <- function(p) {
  list(
    interval = ggplot2::layer_data(p, 2),
    points = ggplot2::layer_data(p, 3)
  )
}

test_that("geom_raincloud() draws a half violin, an interval, and the points", {
  p <- vertical + geom_raincloud()
  expect_length(p$layers, 3L)

  # half violins on the right of each position
  slabs <- slab_measure(p)
  expect_identical(nrow(slabs), 3L)
  expect_true(all(slabs$lo >= slabs$pos - 1e-6))
  expect_true(all(slabs$hi <= slabs$pos + 0.45 + 1e-6))

  ld <- raincloud_layers(p)
  # the median and the 66% and 95% intervals at each position
  for (i in 1:3) {
    v <- iris$Sepal.Length[as.integer(iris$Species) == i]
    rows <- ld$interval[ld$interval$group == i, ]
    expect_setequal(round(rows$.width, 2), c(0.66, 0.95))
    expect_equal(rows$y, rep(stats::median(v), 2), tolerance = 1e-8)
    expect_equal(as.numeric(rows$x), c(i, i))
  }

  # one point per observation, left of its position
  pts <- ld$points
  expect_identical(nrow(pts), nrow(iris))
  pos <- as.integer(iris$Species)
  expect_identical(as.vector(pts$group), pos)
  expect_true(all(pts$x >= pos - 0.45 - 1e-9))
  expect_true(all(pts$x <= pos - 0.05 + 1e-9))
  # the points are spread over the band, not stacked at one place
  expect_gt(diff(range(pts$x - pos)), 0.3)
})

test_that("geom_raincloud() draws horizontal rainclouds", {
  p <- horizontal + geom_raincloud()

  slabs <- slab_measure(p, horizontal = TRUE)
  expect_identical(nrow(slabs), 3L)
  expect_true(all(slabs$lo >= slabs$pos - 1e-6))
  expect_true(all(slabs$hi <= slabs$pos + 0.45 + 1e-6))

  ld <- raincloud_layers(p)
  for (i in 1:3) {
    v <- iris$Sepal.Length[as.integer(iris$Species) == i]
    rows <- ld$interval[ld$interval$group == i, ]
    expect_equal(rows$x, rep(stats::median(v), 2), tolerance = 1e-8)
    expect_equal(as.numeric(rows$y), c(i, i))
  }

  pts <- ld$points
  pos <- as.integer(iris$Species)
  expect_identical(nrow(pts), nrow(iris))
  expect_true(all(pts$y >= pos - 0.45 - 1e-9))
  expect_true(all(pts$y <= pos - 0.05 + 1e-9))
  expect_gt(diff(range(pts$y - pos)), 0.3)
})

test_that("geom_raincloud() keeps the values and follows the seed", {
  for (orientation in c("vertical", "horizontal")) {
    base <- if (orientation == "vertical") vertical else horizontal
    value_axis <- if (orientation == "vertical") "y" else "x"
    cat_axis <- if (orientation == "vertical") "x" else "y"

    one <- ggplot2::layer_data(base + geom_raincloud(seed = 3), 3)
    two <- ggplot2::layer_data(base + geom_raincloud(seed = 3), 3)
    other <- ggplot2::layer_data(base + geom_raincloud(seed = 4), 3)

    expect_identical(one[[value_axis]], iris$Sepal.Length)
    expect_identical(one[[cat_axis]], two[[cat_axis]])
    # a different seed gives a different jitter
    expect_false(identical(one[[cat_axis]], other[[cat_axis]]))
  }
})

test_that("geom_raincloud(shade = ) shades the quantile intervals", {
  w <- c(0.5, 0.8, 0.95)
  for (orientation in c("vertical", "horizontal")) {
    base <- if (orientation == "vertical") vertical else horizontal
    lo_val <- if (orientation == "vertical") "ymin" else "xmin"
    hi_val <- if (orientation == "vertical") "ymax" else "xmax"
    lo_cat <- if (orientation == "vertical") "xmin" else "ymin"
    hi_cat <- if (orientation == "vertical") "xmax" else "ymax"

    p <- base + geom_raincloud(shade = w)
    expect_length(p$layers, 4L)
    expect_s3_class(p$layers[[1]]$geom, "GeomRect")
    expect_s3_class(p$layers[[4]]$geom, "GeomPoint")

    ld <- ggplot2::layer_data(p, 1)
    expect_identical(nrow(ld), 9L)
    for (i in 1:3) {
      v <- iris$Sepal.Length[as.integer(iris$Species) == i]
      rows <- ld[ld$group == i, ]
      rows <- rows[order(rows$coverage), ]
      expect_equal(rows$coverage, w)
      for (k in seq_along(w)) {
        q <- stats::quantile(v, c((1 - w[k]) / 2, (1 + w[k]) / 2), type = 7)
        expect_equal(
          c(rows[[lo_val]][k], rows[[hi_val]][k]),
          unname(q),
          tolerance = 1e-10
        )
      }
      expect_equal(rows[[lo_cat]], rep(i - 0.45, 3), tolerance = 1e-10)
      expect_equal(rows[[hi_cat]], rep(i - 0.05, 3), tolerance = 1e-10)
    }
  }

  # no shading layer by default
  p <- vertical + geom_raincloud()
  expect_false(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomRect"),
    logical(1)
  )))
})

test_that("geom_raincloud(shade = ) skips missing values", {
  d <- data.frame(g = "a", y = c(1:9, NA))
  p <- ggplot2::ggplot(d, ggplot2::aes(g, y)) + geom_raincloud(shade = 0.5)
  ld <- suppressWarnings(ggplot2::layer_data(p, 1))
  expect_equal(
    c(ld$ymin, ld$ymax),
    unname(stats::quantile(1:9, c(0.25, 0.75), type = 7))
  )
})

test_that("geom_raincloud() checks its arguments", {
  expect_error(geom_raincloud(shade = "a"), regexp = "shade")
  expect_error(geom_raincloud(shade = c(0.5, NA)), regexp = "shade")
  expect_error(geom_raincloud(shade = 1.2), regexp = "shade")
  expect_error(geom_raincloud(shade = 0), regexp = "shade")
  expect_error(geom_raincloud(shade = 1), regexp = "shade")
  expect_error(geom_raincloud(interval = "yes"), regexp = "interval")
  expect_error(geom_raincloud(interval = NA), regexp = "interval")
  expect_error(geom_raincloud(seed = "a"), regexp = "seed")
  expect_error(geom_raincloud(seed = c(1, 2)), regexp = "seed")
  expect_error(geom_raincloud(seed = NA), regexp = "seed")
  expect_error(geom_raincloud(flip = TRUE), regexp = "flip")
})

test_that("geom_raincloud() draws small groups without a half violin", {
  d <- data.frame(
    g = c("a", "a", "a", "b", "c", "c", "c"),
    y = c(1, 2, 3, 5, 4, 4, 4)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(g, y)) +
    geom_raincloud(shade = c(0.5, 0.9))
  expect_warning(ggplot2::layer_data(p, 2), "fewer than two")

  slab <- suppressWarnings(ggplot2::layer_data(p, 2))
  expect_false(anyNA(slab$thickness[slab$group == 1]))
  expect_true(all(is.na(slab$thickness[slab$group == 2])))
  expect_true(all(is.na(slab$thickness[slab$group == 3])))

  interval <- suppressWarnings(ggplot2::layer_data(p, 3))
  expect_equal(unique(interval$y[interval$group == 2]), 5)
  expect_equal(unique(interval$y[interval$group == 3]), 4)

  pts <- suppressWarnings(ggplot2::layer_data(p, 4))
  expect_identical(pts$y, d$y)
  expect_identical(sum(pts$group == 2), 1L)

  shade <- suppressWarnings(ggplot2::layer_data(p, 1))
  small <- shade[shade$group %in% 2:3, ]
  expect_identical(nrow(small), 4L)
  expect_equal(small$ymin, small$ymax)
})

test_that("geom_raincloud() looks right", {
  skip_if_not_installed("vdiffr")
  p <- ggplot2::ggplot(
    iris,
    ggplot2::aes(Species, Sepal.Length, fill = Species, colour = Species)
  ) +
    geom_raincloud(shade = c(0.5, 0.95)) +
    theme_modern()
  vdiffr::expect_doppelganger("raincloud with shading", p)
})
