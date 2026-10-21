# the drawn polygons of a layer, in scale units: one element per polygon, with
# the x and y of its vertices in drawing order
drawn_beams <- function(p, layer = 1, panel = 1) {
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[panel]]
  grob <- ggplot2::layer_grob(p, layer)[[panel]]
  to_data <- function(v, range) range[1] + as.numeric(v) * diff(range)
  x <- to_data(grob$x, pp$x.range)
  y <- to_data(grob$y, pp$y.range)
  lapply(split(seq_along(x), grob$id), function(i) list(x = x[i], y = y[i]))
}

# the corners each row of the built data asks for: (x, y), (xend, ymin),
# (xend, ymax), in this order
expected_beams <- function(p, layer = 1) {
  d <- ggplot2::layer_data(p, layer)
  d <- d[stats::complete.cases(d[c("x", "y", "xend", "ymin", "ymax")]), ]
  lapply(seq_len(nrow(d)), function(i) {
    list(
      x = as.numeric(c(d$x[i], d$xend[i], d$xend[i])),
      y = c(d$y[i], d$ymin[i], d$ymax[i])
    )
  })
}

expect_beams <- function(p, layer = 1) {
  drawn <- drawn_beams(p, layer)
  expected <- expected_beams(p, layer)
  expect_length(drawn, length(expected))
  for (i in seq_along(expected)) {
    expect_equal(drawn[[i]]$x, expected[[i]]$x, tolerance = 1e-8)
    expect_equal(drawn[[i]]$y, expected[[i]]$y, tolerance = 1e-8)
  }
}

beam_aes <- ggplot2::aes(x = x, y = y, xend = xend, ymin = ymin, ymax = ymax)

# the second row has ymin above ymax, which the geom draws as given
beams_discrete <- data.frame(
  x = factor(c("a", "a", "b"), levels = c("a", "b", "c")),
  y = c(1, 1, 2),
  xend = factor(c("b", "c", "c"), levels = c("a", "b", "c")),
  ymin = c(0, 4, 1),
  ymax = c(3, 2, 5)
)

beams_continuous <- data.frame(
  x = c(1, 1, 2.5),
  y = c(1, 1, 2),
  xend = c(2.5, 4, 4),
  ymin = c(0, 4, 1),
  ymax = c(3, 2, 5)
)


test_that("geom_lightbeam() is exported", {
  expect_true("geom_lightbeam" %in% getNamespaceExports("see"))
})


test_that("geom_lightbeam() draws one triangle per row on a discrete x axis", {
  p <- ggplot2::ggplot(beams_discrete, beam_aes) + geom_lightbeam()
  expect_beams(p)

  # the corners in scale units: a, b, and c are at 1, 2, and 3
  drawn <- drawn_beams(p)
  expect_equal(drawn[[1]]$x, c(1, 2, 2), tolerance = 1e-8)
  expect_equal(drawn[[1]]$y, c(1, 0, 3), tolerance = 1e-8)
  expect_equal(drawn[[2]]$x, c(1, 3, 3), tolerance = 1e-8)
  expect_equal(drawn[[2]]$y, c(1, 4, 2), tolerance = 1e-8)
  expect_equal(drawn[[3]]$x, c(2, 3, 3), tolerance = 1e-8)
  expect_equal(drawn[[3]]$y, c(2, 1, 5), tolerance = 1e-8)
})


test_that("geom_lightbeam() draws one triangle per row on a continuous x axis", {
  p <- ggplot2::ggplot(beams_continuous, beam_aes) + geom_lightbeam()
  expect_beams(p)

  drawn <- drawn_beams(p)
  expect_equal(drawn[[2]]$x, c(1, 4, 4), tolerance = 1e-8)
  expect_equal(drawn[[2]]$y, c(1, 4, 2), tolerance = 1e-8)
  expect_equal(drawn[[3]]$x, c(2.5, 4, 4), tolerance = 1e-8)
  expect_equal(drawn[[3]]$y, c(2, 1, 5), tolerance = 1e-8)
})


test_that("the tips of geom_lightbeam() go through the y scale", {
  d <- beams_continuous
  d$y <- c(10, 10, 100)
  d$ymin <- c(1, 1000, 10)
  d$ymax <- c(100, 100, 1000)
  p <- ggplot2::ggplot(d, beam_aes) +
    geom_lightbeam() +
    ggplot2::scale_y_log10()

  built <- ggplot2::layer_data(p)
  expect_equal(built$ymin, log10(d$ymin), tolerance = 1e-8)
  expect_equal(built$ymax, log10(d$ymax), tolerance = 1e-8)
  expect_equal(built$y, log10(d$y), tolerance = 1e-8)
  expect_beams(p)
  expect_equal(drawn_beams(p)[[2]]$y, c(1, 3, 2), tolerance = 1e-8)
})


test_that("a level that only xend holds is on the discrete x axis", {
  d <- data.frame(
    x = factor(c("a", "b")),
    y = c(1, 2),
    xend = c("b", "z"),
    ymin = c(0, 1),
    ymax = c(2, 3),
    stringsAsFactors = FALSE
  )
  p <- ggplot2::ggplot(d, beam_aes) + geom_lightbeam()
  x_scale <- ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]
  expect_identical(x_scale$get_limits(), c("a", "b", "z"))
  expect_beams(p)
  expect_equal(drawn_beams(p)[[2]]$x, c(2, 3, 3), tolerance = 1e-8)
})


test_that("geom_lightbeam() has alpha 0.1 and the other defaults of geom_polygon()", {
  polygon <- data.frame(px = c(1, 2, 2), py = c(1, 0, 3))
  p <- ggplot2::ggplot(beams_continuous, beam_aes) +
    geom_lightbeam() +
    ggplot2::geom_polygon(
      data = polygon,
      ggplot2::aes(x = px, y = py),
      inherit.aes = FALSE
    )
  beams <- ggplot2::layer_data(p, 1)
  polygons <- ggplot2::layer_data(p, 2)

  expect_identical(unique(beams$alpha), 0.1)
  for (aesthetic in c("fill", "colour", "linewidth", "linetype")) {
    expect_identical(unique(beams[[aesthetic]]), unique(polygons[[aesthetic]]))
  }
})


test_that("geom_lightbeam() drops rows with a missing y, ymin, or ymax", {
  for (aesthetic in c("y", "ymin", "ymax")) {
    d <- beams_discrete
    d[[aesthetic]][2] <- NA
    p <- ggplot2::ggplot(d, beam_aes) + geom_lightbeam()

    w <- expect_warning(drawn_beams(p))
    expect_match(conditionMessage(w), "Removed 1 row containing missing values")
    expect_match(conditionMessage(w), "geom_lightbeam()", fixed = TRUE)

    drawn <- suppressWarnings(drawn_beams(p))
    expect_length(drawn, 2L)
    expect_equal(drawn[[1]]$x, c(1, 2, 2), tolerance = 1e-8)
    expect_equal(drawn[[1]]$y, c(1, 0, 3), tolerance = 1e-8)
    expect_equal(drawn[[2]]$x, c(2, 3, 3), tolerance = 1e-8)
    expect_equal(drawn[[2]]$y, c(2, 1, 5), tolerance = 1e-8)
  }
})


test_that("geom_lightbeam() drops rows with a missing x or xend on a continuous axis", {
  for (aesthetic in c("x", "xend")) {
    d <- beams_continuous
    d[[aesthetic]][2] <- NA
    p <- ggplot2::ggplot(d, beam_aes) + geom_lightbeam()

    w <- expect_warning(drawn_beams(p))
    expect_match(conditionMessage(w), "Removed 1 row containing missing values")
    expect_match(conditionMessage(w), "geom_lightbeam()", fixed = TRUE)

    drawn <- suppressWarnings(drawn_beams(p))
    expect_length(drawn, 2L)
    expect_equal(drawn[[1]]$x, c(1, 2.5, 2.5), tolerance = 1e-8)
    expect_equal(drawn[[1]]$y, c(1, 0, 3), tolerance = 1e-8)
    expect_equal(drawn[[2]]$x, c(2.5, 4, 4), tolerance = 1e-8)
    expect_equal(drawn[[2]]$y, c(2, 1, 5), tolerance = 1e-8)
  }
})


test_that("geom_lightbeam() draws the rows in their order, whatever their group", {
  d <- data.frame(
    x = c(1, 1),
    y = c(1, 1),
    xend = c(2, 2),
    ymin = c(0, 0.5),
    ymax = c(3, 2),
    g = c(2, 1)
  )
  p <- ggplot2::ggplot(d, beam_aes) +
    geom_lightbeam(ggplot2::aes(group = g))
  expect_identical(as.vector(ggplot2::layer_data(p)$group), c(2L, 1L))

  drawn <- drawn_beams(p)
  expect_equal(drawn[[1]]$y, c(1, 0, 3), tolerance = 1e-8)
  expect_equal(drawn[[2]]$y, c(1, 0.5, 2), tolerance = 1e-8)
})


test_that("a mapped subgroup does not change the beams", {
  d <- beams_continuous
  d$g <- c("a", "b", "a")
  p <- ggplot2::ggplot(d, beam_aes) +
    suppressWarnings(geom_lightbeam(ggplot2::aes(subgroup = g)))
  expect_s3_class(ggplot2::layer_grob(p)[[1]], "polygon")
  expect_beams(p)
})
