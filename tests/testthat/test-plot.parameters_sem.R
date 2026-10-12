skip_if_not_installed("lavaan")
skip_if_not_installed("parameters")

cfa_params <- function() {
  model <- lavaan::cfa(
    "visual =~ x1 + x2 + x3
     textual =~ x4 + x5 + x6",
    data = lavaan::HolzingerSwineford1939
  )
  parameters::model_parameters(model)
}

test_that("data_plot() marks latent factors, not indicators, as latent", {
  dp <- data_plot(cfa_params())
  nodes <- dp$nodes

  expect_setequal(nodes$Name[nodes$Latent], c("visual", "textual"))
  expect_setequal(nodes$Name[!nodes$Latent], paste0("x", 1:6))
  expect_false(anyDuplicated(nodes$Name) > 0)
})

test_that("data_plot() errors clearly when a component has no paths", {
  expect_error(
    data_plot(cfa_params(), component = "regression", ci = FALSE),
    regexp = "No paths to plot"
  )
})
