skip_on_cran()
skip_if_not_installed("vdiffr")
skip_if_not_installed("parameters")
skip_if_not_installed("ggraph")
skip_if_not_installed("tidygraph")

# Helper function for reproducible vdiffr tests
expect_doppelganger_with_seed <- function(title, fig, seed = 123) {
  set.seed(seed)
  vdiffr::expect_doppelganger(title = title, fig = fig)
}

test_that("plot.see_check_model() renders correctly", {
  data(mtcars)
  f <- parameters::factor_analysis(mtcars, n = 3)

  expect_doppelganger_with_seed(
    title = "plot FA, bars-1",
    fig = plot(f)
  )

  expect_doppelganger_with_seed(
    title = "plot FA, bars-2",
    fig = plot(f, colors = c("#E74C3C", "grey85", "#2ECC71"))
  )

  expect_doppelganger_with_seed(
    title = "plot FA, lines",
    fig = plot(f, type = "line")
  )

  expect_doppelganger_with_seed(
    title = "plot FA, graph",
    fig = plot(f, type = "graph-1")
  )

  expect_doppelganger_with_seed(
    title = "plot FA, graph-2",
    fig = plot(
      f,
      type = "graph",
      colors = c("#E74C3C", "grey85", high = "#2ECC71")
    )
  )

  f <- parameters::factor_analysis(mtcars, n = 3, threshold = 0.5)
  expect_doppelganger_with_seed(
    title = "plot FA, graph",
    fig = plot(f, type = "graph-3")
  )

  expect_doppelganger_with_seed(
    title = "plot FA, graph-4",
    fig = plot(
      f,
      type = "graph",
      colors = c("#E74C3C", "grey85", high = "#2ECC71")
    )
  )
})
