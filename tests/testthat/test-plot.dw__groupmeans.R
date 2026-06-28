library(ggplot2)
test_that("`plot.dw_groupmeans_list()` works with a list of length 1", {
  skip_if_not_installed("datawizard")
  data(iris)
  x <- datawizard::means_by_group(iris, "Sepal.Width", "Species")
  p <- plot(x)
  expect_s3_class(p, "gg")
  expect_identical(length(p), 1L)
})

test_that("`plot.dw_groupmeans_list()` works with a list of length 2", {
  skip_if_not_installed("datawizard")
  data(iris)
  x <- datawizard::means_by_group(
    iris,
    c("Sepal.Width", "Sepal.Length"),
    "Species"
  )
  expect_no_error(p <- plot(x))
  expect_s3_class(p, "gg")
  expect_identical(length(p), 1L)
  expect_identical(
    attr(p, "labels")[[2]],
    "\nAnova: R2=0.401; adj.R2=0.393; F=49.160; p<.001\n\nAnova: R2=0.619; adj.R2=0.614; F=119.265; p<.001\n"
  )
})


test_that("`plot.dw_groupmeans()` with no dots arguments works", {
  skip_if_not_installed("datawizard")
  data(iris)
  x <- datawizard::means_by_group(iris$Sepal.Width, iris$Species)
  expect_no_error(p <- plot(x))
  expect_s3_class(p, "gg")
  expect_equal(
    attr(p, "labels")[[2]],
    "\nAnova: R2=0.401; adj.R2=0.393; F=49.160; p<.001\n"
  )
})

test_that("`plot.dw_groupmeans_list()` with a list of length 1
          has the same structe as `plot.dw_groupmeans()` for the same
          data and variables", {
  skip_if_not_installed("datawizard")
  data(iris)
  x1 <- datawizard::means_by_group(iris, "Sepal.Width", "Species")
  p_x1 <- plot(x1, caption = TRUE)
  x2 <- datawizard::means_by_group(iris$Sepal.Width, iris$Species)
  p_x2 <- plot(x2)
  expect_equal(all.equal(p_x1, p_x2), TRUE)
})
