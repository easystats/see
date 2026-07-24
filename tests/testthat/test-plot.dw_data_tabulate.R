test_that("`plot.datawizard_table()` works with single table", {
  x <- datawizard::data_tabulate(mtcars, select = "cyl")
  expect_s3_class(plot(x), "gg")
})

test_that("`plot.datawizard_table()` works with multiple tables", {
  x <- datawizard::data_tabulate(mtcars, select = c("cyl", "carb", "am"))
  expect_true(is.list(plot(x)))
})

test_that("`plot.datawizard_table()` snapshot", {
  skip_if_not_installed("vdiffr")

  x <- datawizard::data_tabulate(mtcars, select = "cyl")
  p <- plot(x)
  expect_s3_class(p, c("gg", "ggplot"))

  vdiffr::expect_doppelganger(
    title = "plot.data_tabulate",
    fig = p
  )
})
