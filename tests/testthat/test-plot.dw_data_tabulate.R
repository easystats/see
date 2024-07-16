test_that("`plot.datawizard_table()` works with single table", {
  x <- datawizard::data_tabulate(mtcars, select = "cyl")
  expect_s3_class(plot(x), "gg")
})

test_that("`plot.datawizard_table()` works with multiple tables", {
  x <- datawizard::data_tabulate(mtcars, select = c("cyl", "carb", "am"))
  expect_true(is.list(plot(x)))
})
