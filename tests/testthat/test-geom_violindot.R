if (getRversion() < "4.1") {
  test_that("`geom_violindot()` works - vdiffr", {
    library(ggplot2)
    library(see)

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "geom_violindot()` works",
      fig = ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
        geom_violindot() +
        theme_modern()
    )
  })
}
