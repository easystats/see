test_that("lightbeam geoms work correctly", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("vdiffr")

  beams <- data.frame(
    x = factor(c("a", "a", "b"), levels = c("a", "b", "c")),
    y = c(1, 1, 2),
    xend = factor(c("b", "c", "c"), levels = c("a", "b", "c")),
    ymin = c(0, 4, 1),
    ymax = c(3, 2, 5)
  )

  p <- ggplot2::ggplot(
    beams,
    ggplot2::aes(x = x, y = y, xend = xend, ymin = ymin, ymax = ymax)
  ) +
    geom_lightbeam()

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "geom_lightbeam_1",
    fig = p
  )
})
