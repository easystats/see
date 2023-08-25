test_that("`plot.dw_data_tabulate()` works with single table", {
  x <- datawizard::data_tabulate(mtcars, select = "cyl")
  expect_s3_class(plot(x), "gg")
})

test_that("`plot.dw_data_tabulate()` works with multiple tables", {
  x <- datawizard::data_tabulate(mtcars, select = c("cyl", "carb", "am"))
  expect_true(is.list(plot(x)))
})
