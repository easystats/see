# the drawn polygons of a layer, in scale units: one element per polygon, with
# the x and y of its vertices in drawing order
drawn_beams <- function(p, layer = 1) {
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  grob <- ggplot2::layer_grob(p, layer)[[1]]
  to_data <- function(v, range) range[1] + as.numeric(v) * diff(range)
  x <- to_data(grob$x, pp$x.range)
  y <- to_data(grob$y, pp$y.range)
  lapply(split(seq_along(x), grob$id), function(i) list(x = x[i], y = y[i]))
}

beam_aes <- ggplot2::aes(x = x, y = y, xend = xend, ymin = ymin, ymax = ymax)

beams <- data.frame(
  x = factor(c("a", "a", "b"), levels = c("a", "b", "c")),
  y = c(1, 1, 2),
  xend = factor(c("b", "c", "c"), levels = c("a", "b", "c")),
  ymin = c(0, 4, 1),
  ymax = c(3, 2, 5)
)


test_that("geom_lightbeam() draws one triangle per row", {
  p <- ggplot2::ggplot(beams, beam_aes) + geom_lightbeam()
  drawn <- drawn_beams(p)
  expect_length(drawn, 3)
  expect_equal(drawn[[1]], list(x = c(1, 2, 2), y = c(1, 0, 3)), tolerance = 1e-8)
  expect_equal(drawn[[2]], list(x = c(1, 3, 3), y = c(1, 4, 2)), tolerance = 1e-8)
  expect_equal(drawn[[3]], list(x = c(2, 3, 3), y = c(2, 1, 5)), tolerance = 1e-8)
  expect_identical(unique(ggplot2::layer_data(p)$alpha), 0.1)
})


test_that("geom_lightbeam() drops rows with missing values", {
  d <- beams
  d$ymin[2] <- NA
  p <- ggplot2::ggplot(d, beam_aes) + geom_lightbeam()
  expect_warning(drawn <- drawn_beams(p), "Removed 1 row")
  expect_length(drawn, 2)
})


test_that("geom_lightbeam() draws the rows in their order, whatever their group", {
  d <- beams[1:2, ]
  d$g <- c(2, 1)
  p <- ggplot2::ggplot(d, beam_aes) + geom_lightbeam(ggplot2::aes(group = g))
  drawn <- drawn_beams(p)
  expect_equal(drawn[[1]]$y, c(1, 0, 3), tolerance = 1e-8)
  expect_equal(drawn[[2]]$y, c(1, 4, 2), tolerance = 1e-8)
})


test_that("a mapped subgroup does not change the beams", {
  d <- beams
  d$g <- c("a", "b", "a")
  p <- ggplot2::ggplot(d, beam_aes) +
    suppressWarnings(geom_lightbeam(ggplot2::aes(subgroup = g)))
  expect_s3_class(ggplot2::layer_grob(p)[[1]], "polygon")
  expect_length(drawn_beams(p), 3)
})
