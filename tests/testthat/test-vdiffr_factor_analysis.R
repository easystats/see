skip_on_cran()
skip_if_not_installed("vdiffr")
skip_if_not_installed("parameters")
skip_if_not_installed("ggraph")
skip_if_not_installed("tidygraph")
skip_if_not_installed("psych")

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
    title = "plot FA, graph-1",
    fig = plot(f, type = "graph")
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
    title = "plot FA, graph-3",
    fig = plot(f, type = "graph")
  )

  expect_doppelganger_with_seed(
    title = "plot FA, graph-4",
    fig = plot(
      f,
      type = "graph",
      colors = c("#E74C3C", "grey85", high = "#2ECC71")
    )
  )

  expect_doppelganger_with_seed(
    title = "plot FA, graph-5",
    fig = plot(
      f,
      type = "graph",
      arrow_end_gap = 0.15,
      factor_node_size = c(20, 45),
      margins = c(0.1, 0.1),
      names_factors = list(MR1 = "First", MR2 = "Second", MR3 = "Third"),
      fill_variables = "purple",
      fill_factors = "green"
    )
  )

  f <- parameters::principal_components(mtcars, n = 3)
  expect_doppelganger_with_seed(
    title = "plot PCA, graph-1",
    fig = plot(f, type = "graph")
  )
})
