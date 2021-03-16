test_that("`geom_point2()` works - vdiffr", {
  library(ggplot2)
  library(see)
  normal <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
    geom_point(size = 8, alpha = 0.3) +
    theme_modern()
  new <- ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
    geom_point2(size = 8, alpha = 0.3) +
    theme_modern()

  vdiffr::expect_doppelganger(
    title = "geom_point2()` works",
    fig = plots(normal, new, n_columns = 2)
  )
})
