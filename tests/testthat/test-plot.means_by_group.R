skip_if_not_installed("datawizard")

test_that("plot.see_dw_groupmeans() flips by default and not with flip = FALSE", {
  x <- datawizard::means_by_group(iris$Sepal.Width, iris$Species)
  expect_s3_class(plot(x)$coordinates, "CoordFlip")
  expect_false(inherits(plot(x, flip = FALSE)$coordinates, "CoordFlip"))
})

test_that("plot.see_dw_groupmeans_list() passes flip on", {
  x <- datawizard::means_by_group(
    iris,
    c("Sepal.Width", "Petal.Width"),
    "Species"
  )
  expect_s3_class(plot(x)$coordinates, "CoordFlip")
  expect_false(inherits(plot(x, flip = FALSE)$coordinates, "CoordFlip"))

  x1 <- datawizard::means_by_group(iris, "Sepal.Width", "Species")
  expect_false(inherits(plot(x1, flip = FALSE)$coordinates, "CoordFlip"))
})

test_that("plot.see_dw_groupmeans() rejects a non-logical flip", {
  x <- datawizard::means_by_group(iris$Sepal.Width, iris$Species)
  expect_error(
    plot(x, flip = "yes"),
    regexp = "must be either `TRUE` or `FALSE`"
  )
  expect_error(plot(x, flip = NA), regexp = "must be either `TRUE` or `FALSE`")
})
